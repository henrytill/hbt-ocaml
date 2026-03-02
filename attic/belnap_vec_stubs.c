#include <stdint.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if !defined(__has_builtin) || !__has_builtin(__builtin_popcountll)
#    error "__builtin_popcountll is required"
#endif

#if !defined(__has_builtin) || !__has_builtin(__builtin_ctzll)
#    error "__builtin_ctzll is required"
#endif

struct belnap_vec;

#define Bv_val(v) ((struct belnap_vec *)Data_custom_val(v))

/* Must match bits_log2 / bits_mask in belnap_vec.ml. */
enum
{
    BITS_LOG2 = 6,
    BITS_MASK = (1 << BITS_LOG2) - 1
};

static uint64_t const ALL_ONES = ~UINT64_C(0);

/* Layout: words[2*i] = pos plane, words[2*i+1] = neg plane for word-pair i.
   nwords is the per-plane word count = ceil(width/64).
   The flat array has 2*nwords uint64_t elements total. */

struct belnap_vec
{
    int nwords;       /* per-plane word count = ceil(width/64) */
    uint64_t words[]; /* flexible array member; 2*nwords elements */
};

static struct custom_operations bv_ops = {
    "hbt-attic.belnap_vec",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

static inline size_t bv_words_bytes(int nw)
{
    return 2 * (size_t)(nw) * sizeof(uint64_t);
}

CAMLprim value bv_alloc(value vnw)
{
    CAMLparam1(vnw);
    CAMLlocal1(v);
    int const nwords = Int_val(vnw);
    mlsize_t const bsz = sizeof(struct belnap_vec) + bv_words_bytes(nwords);
    v = caml_alloc_custom(&bv_ops, bsz, 0, 1);
    Bv_val(v)->nwords = nwords;
    memset(Bv_val(v)->words, 0, bv_words_bytes(nwords));
    CAMLreturn(v);
}

CAMLprim value bv_blit_grow(value vsrc, value vnew_nw)
{
    CAMLparam2(vsrc, vnew_nw);
    CAMLlocal1(vdst);
    int const new_nwords = Int_val(vnew_nw);
    mlsize_t const bsz = sizeof(struct belnap_vec) + bv_words_bytes(new_nwords);
    vdst = caml_alloc_custom(&bv_ops, bsz, 0, 1);
    struct belnap_vec *dst = Bv_val(vdst);
    struct belnap_vec const *src = Bv_val(vsrc);
    int const old_nwords = src->nwords;
    dst->nwords = new_nwords;
    memcpy(dst->words, src->words, bv_words_bytes(old_nwords));
    memset(dst->words + (2 * old_nwords), 0, bv_words_bytes(new_nwords - old_nwords));
    CAMLreturn(vdst);
}

CAMLprim value bv_get(value vbv, value vi)
{
    CAMLparam2(vbv, vi);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const i = Int_val(vi);

    int const word = i >> BITS_LOG2;
    int const bit = i & BITS_MASK;
    int const pos_bit = (int)((w[word * 2] >> bit) & 1);
    int const neg_bit = (int)((w[word * 2 + 1] >> bit) & 1);
    CAMLreturn(Val_int((neg_bit << 1) | pos_bit));
}

CAMLprim value bv_set(value vbv, value vi, value vraw)
{
    CAMLparam3(vbv, vi, vraw);
    uint64_t *const w = Bv_val(vbv)->words;
    int const i = Int_val(vi);
    int const raw = Int_val(vraw);

    int const word = i >> BITS_LOG2;
    int const bit = i & BITS_MASK;
    uint64_t const mask = UINT64_C(1) << bit;
    uint64_t const pos = (uint64_t)(raw & 1) << bit;
    uint64_t const neg = (uint64_t)((raw >> 1) & 1) << bit;
    w[word * 2] = (w[word * 2] & ~mask) | pos;
    w[word * 2 + 1] = (w[word * 2 + 1] & ~mask) | neg;
    CAMLreturn(Val_unit);
}

CAMLprim value bv_mask_tail(value vbv, value vwidth)
{
    CAMLparam2(vbv, vwidth);
    uint64_t *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;
    int const width = Int_val(vwidth);

    int const r = width & BITS_MASK;
    if (nw > 0 && r != 0)
    {
        uint64_t const mask = (UINT64_C(1) << r) - 1;
        int const base = (nw - 1) * 2;
        w[base] &= mask;
        w[base + 1] &= mask;
    }
    CAMLreturn(Val_unit);
}

CAMLprim value bv_fill(value vbv, value vfrom, value vto, value vraw)
{
    CAMLparam4(vbv, vfrom, vto, vraw);
    uint64_t *const w = Bv_val(vbv)->words;
    int const from = Int_val(vfrom);
    int const to_ = Int_val(vto);
    int const raw = Int_val(vraw);

    uint64_t const pos = (raw & 1) ? ALL_ONES : UINT64_C(0);
    uint64_t const neg = (raw >> 1) & 1 ? ALL_ONES : UINT64_C(0);

    for (int i = from; i < to_; i++)
    {
        w[i * 2] = pos;
        w[i * 2 + 1] = neg;
    }

    CAMLreturn(Val_unit);
}

CAMLprim value bv_not(value src, value dst)
{
    CAMLparam2(src, dst);
    uint64_t const *const s = Bv_val(src)->words;
    uint64_t *const d = Bv_val(dst)->words;
    int const nw = Bv_val(src)->nwords;

    for (int i = 0; i < nw; i++)
    {
        d[i * 2] = s[i * 2 + 1];
        d[i * 2 + 1] = s[i * 2];
    }

    CAMLreturn(Val_unit);
}

CAMLprim value bv_and(value va, value vb, value dst)
{
    CAMLparam3(va, vb, dst);
    uint64_t const *const a = Bv_val(va)->words;
    uint64_t const *const b = Bv_val(vb)->words;
    uint64_t *const d = Bv_val(dst)->words;
    int const nwa = Bv_val(va)->nwords;
    int const nwb = Bv_val(vb)->nwords;

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        uint64_t const ap = i < nwa ? a[i * 2] : 0;
        uint64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        uint64_t const bp = i < nwb ? b[i * 2] : 0;
        uint64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap & bp;
        d[i * 2 + 1] = an | bn;
    }

    CAMLreturn(Val_unit);
}

CAMLprim value bv_or(value va, value vb, value dst)
{
    CAMLparam3(va, vb, dst);
    uint64_t const *const a = Bv_val(va)->words;
    uint64_t const *const b = Bv_val(vb)->words;
    uint64_t *const d = Bv_val(dst)->words;
    int const nwa = Bv_val(va)->nwords;
    int const nwb = Bv_val(vb)->nwords;

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        uint64_t const ap = i < nwa ? a[i * 2] : 0;
        uint64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        uint64_t const bp = i < nwb ? b[i * 2] : 0;
        uint64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap | bp;
        d[i * 2 + 1] = an & bn;
    }

    CAMLreturn(Val_unit);
}

CAMLprim value bv_merge(value va, value vb, value dst)
{
    CAMLparam3(va, vb, dst);
    uint64_t const *const a = Bv_val(va)->words;
    uint64_t const *const b = Bv_val(vb)->words;
    uint64_t *const d = Bv_val(dst)->words;
    int const nwa = Bv_val(va)->nwords;
    int const nwb = Bv_val(vb)->nwords;

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        uint64_t const ap = i < nwa ? a[i * 2] : 0;
        uint64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        uint64_t const bp = i < nwb ? b[i * 2] : 0;
        uint64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap | bp;
        d[i * 2 + 1] = an | bn;
    }

    CAMLreturn(Val_unit);
}

CAMLprim value bv_consensus(value va, value vb, value dst)
{
    CAMLparam3(va, vb, dst);
    uint64_t const *const a = Bv_val(va)->words;
    uint64_t const *const b = Bv_val(vb)->words;
    uint64_t *const d = Bv_val(dst)->words;
    int const nwa = Bv_val(va)->nwords;
    int const nwb = Bv_val(vb)->nwords;

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        uint64_t const ap = i < nwa ? a[i * 2] : 0;
        uint64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        uint64_t const bp = i < nwb ? b[i * 2] : 0;
        uint64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap & bp;        /* pos plane: AND */
        d[i * 2 + 1] = an & bn;   /* neg plane: AND */
    }

    CAMLreturn(Val_unit);
}

static inline uint64_t tail_mask(int const width)
{
    int const r = width & BITS_MASK;
    return r == 0 ? ALL_ONES : (UINT64_C(1) << r) - 1;
}

CAMLprim value bv_is_consistent(value vbv, value vwidth)
{
    CAMLparam2(vbv, vwidth);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        uint64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        if ((w[i * 2] & w[i * 2 + 1] & m) != 0)
            CAMLreturn(Val_int(0));
    }

    CAMLreturn(Val_int(1));
}

CAMLprim value bv_is_all_determined(value vbv, value vwidth)
{
    CAMLparam2(vbv, vwidth);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        uint64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        uint64_t const xorv = w[i * 2] ^ w[i * 2 + 1];
        if ((xorv & m) != m)
            CAMLreturn(Val_int(0));
    }

    CAMLreturn(Val_int(1));
}

CAMLprim value bv_is_all_true(value vbv, value vwidth)
{
    CAMLparam2(vbv, vwidth);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        uint64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        if ((w[i * 2] & m) != m)
            CAMLreturn(Val_int(0)); /* not all pos set */
        if ((w[i * 2 + 1] & m) != 0)
            CAMLreturn(Val_int(0)); /* some neg set */
    }

    CAMLreturn(Val_int(1));
}

CAMLprim value bv_is_all_false(value vbv, value vwidth)
{
    CAMLparam2(vbv, vwidth);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        uint64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        if ((w[i * 2] & m) != 0)
            CAMLreturn(Val_int(0)); /* some pos set */
        if ((w[i * 2 + 1] & m) != m)
            CAMLreturn(Val_int(0)); /* not all neg set */
    }

    CAMLreturn(Val_int(1));
}

static inline int popcount64(uint64_t x)
{
    return __builtin_popcountll(x);
}

CAMLprim value bv_count_true(value vbv)
{
    CAMLparam1(vbv);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;

    int n = 0;
    for (int i = 0; i < nw; i++)
        n += popcount64(w[i * 2] & ~w[i * 2 + 1]);
    CAMLreturn(Val_int(n));
}

CAMLprim value bv_count_false(value vbv)
{
    CAMLparam1(vbv);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;

    int n = 0;
    for (int i = 0; i < nw; i++)
        n += popcount64(~w[i * 2] & w[i * 2 + 1]);
    CAMLreturn(Val_int(n));
}

CAMLprim value bv_count_both(value vbv)
{
    CAMLparam1(vbv);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;

    int n = 0;
    for (int i = 0; i < nw; i++)
        n += popcount64(w[i * 2] & w[i * 2 + 1]);
    CAMLreturn(Val_int(n));
}

CAMLprim value bv_equal(value va, value vb)
{
    CAMLparam2(va, vb);
    struct belnap_vec const *a = Bv_val(va);
    struct belnap_vec const *b = Bv_val(vb);
    if (a->nwords != b->nwords)
        CAMLreturn(Val_int(0));
    CAMLreturn(Val_int(memcmp(a->words, b->words, bv_words_bytes(a->nwords)) == 0));
}

CAMLprim value bv_init_from_list(value vbv, value vlist)
{
    CAMLparam2(vbv, vlist);
    struct belnap_vec *bv = Bv_val(vbv);
    int i = 0;
    uint64_t pos = 0, neg = 0;
    while (vlist != Val_emptylist)
    {
        int const raw = Int_val(Field(vlist, 0));
        int const bit = i & BITS_MASK;
        pos |= (uint64_t)(raw & 1) << bit;
        neg |= (uint64_t)((raw >> 1) & 1) << bit;
        vlist = Field(vlist, 1);
        if (bit == BITS_MASK || vlist == Val_emptylist)
        {
            int const word = i >> BITS_LOG2;
            bv->words[word * 2] = pos;
            bv->words[word * 2 + 1] = neg;
            pos = 0;
            neg = 0;
        }
        i++;
    }
    CAMLreturn(Val_unit);
}

CAMLprim value bv_init_from_array(value vbv, value varr)
{
    CAMLparam2(vbv, varr);
    struct belnap_vec *bv = Bv_val(vbv);
    int const width = (int)Wosize_val(varr);
    uint64_t pos = 0, neg = 0;
    for (int i = 0; i < width; i++)
    {
        int const raw = Int_val(Field(varr, i));
        int const bit = i & BITS_MASK;
        pos |= (uint64_t)(raw & 1) << bit;
        neg |= (uint64_t)((raw >> 1) & 1) << bit;
        if (bit == BITS_MASK || i == width - 1)
        {
            int const word = i >> BITS_LOG2;
            bv->words[word * 2] = pos;
            bv->words[word * 2 + 1] = neg;
            pos = 0;
            neg = 0;
        }
    }
    CAMLreturn(Val_unit);
}

CAMLprim value bv_to_array(value vbv, value vwidth)
{
    CAMLparam2(vbv, vwidth);
    CAMLlocal1(arr);
    int const width = Int_val(vwidth);
    if (width == 0)
        CAMLreturn(Atom(0));
    arr = caml_alloc(width, 0);
    /* Extract raw pointers after allocation so they are not stale if GC ran. */
    struct belnap_vec const *const bv = Bv_val(vbv);
    int const nwords = bv->nwords;
    uint64_t const *const w = bv->words;
    /* No allocation below; Store_field write-barrier is a no-op for immediates. */
    for (int i = 0; i < nwords; i++)
    {
        uint64_t const pos = w[i * 2];
        uint64_t const neg = w[i * 2 + 1];
        int const base = i << BITS_LOG2;
        int const limit = base + 64 < width ? 64 : width - base;
        for (int bit = 0; bit < limit; bit++)
        {
            int const raw = (int)(((pos >> bit) & 1) | (((neg >> bit) & 1) << 1));
            Store_field(arr, base + bit, Val_int(raw));
        }
    }
    CAMLreturn(arr);
}

CAMLprim value bv_find_first(value vbv, value vwidth, value vraw)
{
    CAMLparam3(vbv, vwidth, vraw);
    uint64_t const *const w = Bv_val(vbv)->words;
    int const nw = Bv_val(vbv)->nwords;
    int const width = Int_val(vwidth);
    int const raw = Int_val(vraw);
    uint64_t const want_pos = (uint64_t)(raw & 1);
    uint64_t const want_neg = (uint64_t)((raw >> 1) & 1);

    for (int i = 0; i < nw; i++)
    {
        uint64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        uint64_t const pos_word = w[i * 2];
        uint64_t const neg_word = w[i * 2 + 1];
        /* Match positions where pos == want_pos AND neg == want_neg. */
        uint64_t const pos_match = want_pos ? pos_word : ~pos_word;
        uint64_t const neg_match = want_neg ? neg_word : ~neg_word;
        uint64_t const candidates = pos_match & neg_match & m;
        if (candidates != 0)
            CAMLreturn(Val_int((i << BITS_LOG2) + __builtin_ctzll(candidates)));
    }
    CAMLreturn(Val_int(-1));
}
