#include <stdint.h>

#include <caml/bigarray.h>
#include <caml/mlvalues.h>

#if !defined(__has_builtin) || !__has_builtin(__builtin_popcountll)
#    error "__builtin_popcountll is required"
#endif

/* Must match bits_log2 / BITS_MASK in belnap_vec.ml. */
enum
{
    BITS_LOG2 = 6,
    BITS_MASK = (1 << BITS_LOG2) - 1
};

static int64_t const ALL_ONES = INT64_C(-1);

/* Layout: words[2*i] = pos plane, words[2*i+1] = neg plane for word-pair i.
   All functions are [@@noalloc] — they never allocate OCaml values. */

static inline int popcount64(int64_t x)
{
    return __builtin_popcountll(x);
}

/* bv_get(words, i) -> Val_int(2-bit Belnap encoding) */
CAMLprim value caml_bv_get(value words, value vi)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const i = Int_val(vi);

    int const word = i >> BITS_LOG2;
    int const bit = i & BITS_MASK;
    int const pos_bit = (int)(((uint64_t)w[word * 2] >> bit) & 1);
    int const neg_bit = (int)(((uint64_t)w[word * 2 + 1] >> bit) & 1);
    return Val_int((neg_bit << 1) | pos_bit);
}

/* bv_set(words, i, raw) — raw is Val_int(Belnap.to_bits v) */
CAMLprim value caml_bv_set(value words, value vi, value vraw)
{
    int64_t *const w = Caml_ba_data_val(words);
    int const i = Int_val(vi);
    int const raw = Int_val(vraw);

    int const word = i >> BITS_LOG2;
    int const bit = i & BITS_MASK;
    int64_t const mask = UINT64_C(1) << bit;
    int64_t const pos = (uint64_t)((raw) & 1) << bit;
    int64_t const neg = (uint64_t)((raw >> 1) & 1) << bit;
    w[word * 2] = (w[word * 2] & ~mask) | pos;
    w[word * 2 + 1] = (w[word * 2 + 1] & ~mask) | neg;
    return Val_unit;
}

/* bv_mask_tail(words, nw, width) — clears bits >= (width & 63) in last pair */
CAMLprim value caml_bv_mask_tail(value words, value vnw, value vwidth)
{
    int64_t *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);
    int const width = Int_val(vwidth);

    int const r = width & BITS_MASK;
    if (nw > 0 && r != 0)
    {
        int64_t const mask = (UINT64_C(1) << r) - 1;
        int const base = (nw - 1) * 2;
        w[base] &= mask;
        w[base + 1] &= mask;
    }
    return Val_unit;
}

/* bv_fill(words, from_pair, to_pair, raw_fill) — fills word-pairs [from, to) */
CAMLprim value caml_bv_fill(value words, value vfrom, value vto, value vraw)
{
    int64_t *const w = Caml_ba_data_val(words);
    int const from = Int_val(vfrom);
    int const to_ = Int_val(vto);
    int const raw = Int_val(vraw);

    int64_t const pos = (raw & 1) ? ALL_ONES : INT64_C(0);
    int64_t const neg = (raw >> 1) & 1 ? ALL_ONES : INT64_C(0);

    for (int i = from; i < to_; i++)
    {
        w[i * 2] = pos;
        w[i * 2 + 1] = neg;
    }

    return Val_unit;
}

/* bv_fill_gap(words, old_width, new_width, raw)
   Fills elements [old_width, new_width) with the given Belnap encoding and
   masks the tail beyond new_width. Assumes word-pairs [old_nw, new_nw) are
   already zero-initialized. */
CAMLprim value caml_bv_fill_gap(value vwords, value vold_width, value vnew_width, value vraw)
{
    int64_t *const w = Caml_ba_data_val(vwords);
    int const old_width = Int_val(vold_width);
    int const new_width = Int_val(vnew_width);
    int const raw = Int_val(vraw);

    int const old_nw = (old_width + BITS_MASK) >> BITS_LOG2;
    int const new_nw = (new_width + BITS_MASK) >> BITS_LOG2;

    /* Patch the partial tail of the last old word-pair, if any. */
    if (raw != 0 && old_nw > 0)
    {
        int const r = old_width & BITS_MASK;
        if (r != 0)
        {
            int64_t const fill_mask = ~((UINT64_C(1) << r) - 1);
            int const base = (old_nw - 1) * 2;
            if (raw & 1)
                w[base] |= fill_mask;
            if ((raw >> 1) & 1)
                w[base + 1] |= fill_mask;
        }
    }

    /* Fill whole new word-pairs [old_nw, new_nw). */
    if (new_nw > old_nw)
    {
        int64_t const pos = (raw & 1) ? ALL_ONES : INT64_C(0);
        int64_t const neg = (raw >> 1) & 1 ? ALL_ONES : INT64_C(0);
        for (int i = old_nw; i < new_nw; i++)
        {
            w[i * 2] = pos;
            w[i * 2 + 1] = neg;
        }
    }

    /* Mask the tail of the last new word-pair. */
    if (raw != 0 && new_nw > 0)
    {
        int const r = new_width & BITS_MASK;
        if (r != 0)
        {
            int64_t const mask = (UINT64_C(1) << r) - 1;
            int const base = (new_nw - 1) * 2;
            w[base] &= mask;
            w[base + 1] &= mask;
        }
    }

    return Val_unit;
}

/* bv_not(src, dst, nw) */
CAMLprim value caml_bv_not(value src, value dst, value vnw)
{
    int64_t const *const s = Caml_ba_data_val(src);
    int64_t *const d = Caml_ba_data_val(dst);
    int const nw = Int_val(vnw);

    for (int i = 0; i < nw; i++)
    {
        d[i * 2] = s[i * 2 + 1];
        d[i * 2 + 1] = s[i * 2];
    }

    return Val_unit;
}

/* bv_and(a, b, dst, nwa, nwb) — a/b may be shorter; dst is zero-initialized */
CAMLprim value caml_bv_and(value va, value vb, value dst, value vnwa, value vnwb)
{
    int64_t const *const a = Caml_ba_data_val(va);
    int64_t const *const b = Caml_ba_data_val(vb);
    int64_t *const d = Caml_ba_data_val(dst);
    int const nwa = Int_val(vnwa);
    int const nwb = Int_val(vnwb);

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        int64_t const ap = i < nwa ? a[i * 2] : 0;
        int64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        int64_t const bp = i < nwb ? b[i * 2] : 0;
        int64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap & bp;
        d[i * 2 + 1] = an | bn;
    }

    return Val_unit;
}

/* bv_or(a, b, dst, nwa, nwb) */
CAMLprim value caml_bv_or(value va, value vb, value dst, value vnwa, value vnwb)
{
    int64_t const *const a = Caml_ba_data_val(va);
    int64_t const *const b = Caml_ba_data_val(vb);
    int64_t *const d = Caml_ba_data_val(dst);
    int const nwa = Int_val(vnwa);
    int const nwb = Int_val(vnwb);

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        int64_t const ap = i < nwa ? a[i * 2] : 0;
        int64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        int64_t const bp = i < nwb ? b[i * 2] : 0;
        int64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap | bp;
        d[i * 2 + 1] = an & bn;
    }

    return Val_unit;
}

/* bv_merge(a, b, dst, nwa, nwb) */
CAMLprim value caml_bv_merge(value va, value vb, value dst, value vnwa, value vnwb)
{
    int64_t const *const a = Caml_ba_data_val(va);
    int64_t const *const b = Caml_ba_data_val(vb);
    int64_t *const d = Caml_ba_data_val(dst);
    int const nwa = Int_val(vnwa);
    int const nwb = Int_val(vnwb);

    int const nw = nwa > nwb ? nwa : nwb;

    for (int i = 0; i < nw; i++)
    {
        int64_t const ap = i < nwa ? a[i * 2] : 0;
        int64_t const an = i < nwa ? a[i * 2 + 1] : 0;
        int64_t const bp = i < nwb ? b[i * 2] : 0;
        int64_t const bn = i < nwb ? b[i * 2 + 1] : 0;
        d[i * 2] = ap | bp;
        d[i * 2 + 1] = an | bn;
    }

    return Val_unit;
}

/* Query helpers: build a tail mask for the last word-pair */
static inline int64_t tail_mask(int const width)
{
    int const r = width & BITS_MASK;
    return r == 0 ? ALL_ONES : (UINT64_C(1) << r) - 1;
}

/* bv_is_consistent(words, nw, width) — true if no pos & neg set simultaneously */
CAMLprim value caml_bv_is_consistent(value words, value vnw, value vwidth)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        int64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        if ((w[i * 2] & w[i * 2 + 1] & m) != 0)
            return Val_int(0);
    }

    return Val_int(1);
}

/* bv_is_all_determined(words, nw, width) — true if every live bit has pos XOR neg */
CAMLprim value caml_bv_is_all_determined(value words, value vnw, value vwidth)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        int64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        int64_t const xor = w[i * 2] ^ w[i * 2 + 1];
        if ((xor&m) != m)
            return Val_int(0);
    }

    return Val_int(1);
}

/* bv_is_all_true(words, nw, width) */
CAMLprim value caml_bv_is_all_true(value words, value vnw, value vwidth)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        int64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        if ((w[i * 2] & m) != m)
            return Val_int(0); /* not all pos set */
        if ((w[i * 2 + 1] & m) != 0)
            return Val_int(0); /* some neg set */
    }

    return Val_int(1);
}

/* bv_is_all_false(words, nw, width) */
CAMLprim value caml_bv_is_all_false(value words, value vnw, value vwidth)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);
    int const width = Int_val(vwidth);

    for (int i = 0; i < nw; i++)
    {
        int64_t const m = (i == nw - 1) ? tail_mask(width) : ALL_ONES;
        if ((w[i * 2] & m) != 0)
            return Val_int(0); /* some pos set */
        if ((w[i * 2 + 1] & m) != m)
            return Val_int(0); /* not all neg set */
    }

    return Val_int(1);
}

/* bv_count_true(words, nw) — popcount(pos & ~neg) over all word-pairs */
CAMLprim value caml_bv_count_true(value words, value vnw)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);

    int n = 0;
    for (int i = 0; i < nw; i++)
        n += popcount64(w[i * 2] & ~w[i * 2 + 1]);
    return Val_int(n);
}

/* bv_count_false(words, nw) — popcount(~pos & neg) */
CAMLprim value caml_bv_count_false(value words, value vnw)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);

    int n = 0;
    for (int i = 0; i < nw; i++)
        n += popcount64(~w[i * 2] & w[i * 2 + 1]);
    return Val_int(n);
}

/* bv_count_both(words, nw) — popcount(pos & neg) */
CAMLprim value caml_bv_count_both(value words, value vnw)
{
    int64_t const *const w = Caml_ba_data_val(words);
    int const nw = Int_val(vnw);

    int n = 0;
    for (int i = 0; i < nw; i++)
        n += popcount64(w[i * 2] & w[i * 2 + 1]);
    return Val_int(n);
}
