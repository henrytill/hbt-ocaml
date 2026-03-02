// belnap_vec_stubs.js
//
// js_of_ocaml JavaScript stubs for Belnap vector operations.
//
// Each bv is a plain JS object { nwords, words } where words is a
// BigUint64Array of length 2*nwords in interleaved pos/neg layout:
//   words[2*i] = pos-plane word i, words[2*i+1] = neg-plane word i.
//
// OCaml integers arrive and depart as plain JS numbers (no tagging).
// OCaml unit is 0.  OCaml bool/int results are plain 0/1.
//
// BigInt bitwise NOT (~x) is unbounded two's-complement.  ANDing ~x
// with a BigUint64Array element (at most 64 bits) bounds the result
// naturally, so explicit masking after NOT is only needed when both
// operands could be negative (see bv_find_first).

//Provides: bv_all_ones
var bv_all_ones = 0xffffffffffffffffn;

//Provides: bv_tail_mask
//Requires: bv_all_ones
function bv_tail_mask(width) {
    var r = width & 63;
    return r === 0 ? bv_all_ones : (1n << BigInt(r)) - 1n;
}

//Provides: bv_popcount64
function bv_popcount64(v) {
    var n = 0;
    while (v !== 0n) {
        v &= v - 1n;
        n++;
    }
    return n;
}

//Provides: bv_ctz64
function bv_ctz64(v) {
    var n = 0;
    while ((v & 1n) === 0n) {
        v >>= 1n;
        n++;
    }
    return n;
}

//Provides: bv_alloc
function bv_alloc(nw) {
    return { nwords: nw, words: new globalThis.BigUint64Array(2 * nw) };
}

//Provides: bv_blit_grow
//Requires: bv_alloc
function bv_blit_grow(src, new_nw) {
    var dst = bv_alloc(new_nw);
    var old_nw = src.nwords;
    for (var i = 0; i < 2 * old_nw; i++) {
        dst.words[i] = src.words[i];
    }
    return dst;
}

//Provides: bv_get
function bv_get(bv, i) {
    var word = i >> 6;
    var bit = BigInt(i & 63);
    var pos_bit = Number((bv.words[word * 2] >> bit) & 1n);
    var neg_bit = Number((bv.words[word * 2 + 1] >> bit) & 1n);
    return (neg_bit << 1) | pos_bit;
}

//Provides: bv_set
function bv_set(bv, i, raw) {
    var word = i >> 6;
    var bit = BigInt(i & 63);
    var mask = 1n << bit;
    var pos = BigInt(raw & 1) << bit;
    var neg = BigInt((raw >> 1) & 1) << bit;
    bv.words[word * 2] = (bv.words[word * 2] & ~mask) | pos;
    bv.words[word * 2 + 1] = (bv.words[word * 2 + 1] & ~mask) | neg;
    return 0;
}

//Provides: bv_mask_tail
function bv_mask_tail(bv, width) {
    var nw = bv.nwords;
    var r = width & 63;
    if (nw > 0 && r !== 0) {
        var mask = (1n << BigInt(r)) - 1n;
        var base = (nw - 1) * 2;
        bv.words[base] &= mask;
        bv.words[base + 1] &= mask;
    }
    return 0;
}

//Provides: bv_fill
//Requires: bv_all_ones
function bv_fill(bv, from, to, raw) {
    var pos = raw & 1 ? bv_all_ones : 0n;
    var neg = (raw >> 1) & 1 ? bv_all_ones : 0n;
    for (var i = from; i < to; i++) {
        bv.words[i * 2] = pos;
        bv.words[i * 2 + 1] = neg;
    }
    return 0;
}

//Provides: bv_not
function bv_not(src, dst) {
    var nw = src.nwords;
    for (var i = 0; i < nw; i++) {
        dst.words[i * 2] = src.words[i * 2 + 1];
        dst.words[i * 2 + 1] = src.words[i * 2];
    }
    return 0;
}

//Provides: bv_and
function bv_and(a, b, dst) {
    var nw = a.nwords > b.nwords ? a.nwords : b.nwords;
    for (var i = 0; i < nw; i++) {
        var ap = i < a.nwords ? a.words[i * 2] : 0n;
        var an = i < a.nwords ? a.words[i * 2 + 1] : 0n;
        var bp = i < b.nwords ? b.words[i * 2] : 0n;
        var bn = i < b.nwords ? b.words[i * 2 + 1] : 0n;
        dst.words[i * 2] = ap & bp;
        dst.words[i * 2 + 1] = an | bn;
    }
    return 0;
}

//Provides: bv_or
function bv_or(a, b, dst) {
    var nw = a.nwords > b.nwords ? a.nwords : b.nwords;
    for (var i = 0; i < nw; i++) {
        var ap = i < a.nwords ? a.words[i * 2] : 0n;
        var an = i < a.nwords ? a.words[i * 2 + 1] : 0n;
        var bp = i < b.nwords ? b.words[i * 2] : 0n;
        var bn = i < b.nwords ? b.words[i * 2 + 1] : 0n;
        dst.words[i * 2] = ap | bp;
        dst.words[i * 2 + 1] = an & bn;
    }
    return 0;
}

//Provides: bv_merge
function bv_merge(a, b, dst) {
    var nw = a.nwords > b.nwords ? a.nwords : b.nwords;
    for (var i = 0; i < nw; i++) {
        var ap = i < a.nwords ? a.words[i * 2] : 0n;
        var an = i < a.nwords ? a.words[i * 2 + 1] : 0n;
        var bp = i < b.nwords ? b.words[i * 2] : 0n;
        var bn = i < b.nwords ? b.words[i * 2 + 1] : 0n;
        dst.words[i * 2] = ap | bp;
        dst.words[i * 2 + 1] = an | bn;
    }
    return 0;
}

//Provides: bv_is_consistent
//Requires: bv_all_ones, bv_tail_mask
function bv_is_consistent(bv, width) {
    var nw = bv.nwords;
    for (var i = 0; i < nw; i++) {
        var m = i === nw - 1 ? bv_tail_mask(width) : bv_all_ones;
        if ((bv.words[i * 2] & bv.words[i * 2 + 1] & m) !== 0n) {
            return 0;
        }
    }
    return 1;
}

//Provides: bv_is_all_determined
//Requires: bv_all_ones, bv_tail_mask
function bv_is_all_determined(bv, width) {
    var nw = bv.nwords;
    for (var i = 0; i < nw; i++) {
        var m = i === nw - 1 ? bv_tail_mask(width) : bv_all_ones;
        var xorv = bv.words[i * 2] ^ bv.words[i * 2 + 1];
        if ((xorv & m) !== m) {
            return 0;
        }
    }
    return 1;
}

//Provides: bv_is_all_true
//Requires: bv_all_ones, bv_tail_mask
function bv_is_all_true(bv, width) {
    var nw = bv.nwords;
    for (var i = 0; i < nw; i++) {
        var m = i === nw - 1 ? bv_tail_mask(width) : bv_all_ones;
        if ((bv.words[i * 2] & m) !== m) {
            // not all pos set
            return 0;
        }
        if ((bv.words[i * 2 + 1] & m) !== 0n) {
            // some neg set
            return 0;
        }
    }
    return 1;
}

//Provides: bv_is_all_false
//Requires: bv_all_ones, bv_tail_mask
function bv_is_all_false(bv, width) {
    var nw = bv.nwords;
    for (var i = 0; i < nw; i++) {
        var m = i === nw - 1 ? bv_tail_mask(width) : bv_all_ones;
        if ((bv.words[i * 2] & m) !== 0n) {
            // some pos set
            return 0;
        }
        if ((bv.words[i * 2 + 1] & m) !== m) {
            // not all neg set
            return 0;
        }
    }
    return 1;
}

//Provides: bv_count_true
//Requires: bv_popcount64
function bv_count_true(bv) {
    var n = 0;
    for (var i = 0; i < bv.nwords; i++) {
        n += bv_popcount64(bv.words[i * 2] & ~bv.words[i * 2 + 1]);
    }
    return n;
}

//Provides: bv_count_false
//Requires: bv_popcount64
function bv_count_false(bv) {
    var n = 0;
    for (var i = 0; i < bv.nwords; i++) {
        n += bv_popcount64(~bv.words[i * 2] & bv.words[i * 2 + 1]);
    }
    return n;
}

//Provides: bv_count_both
//Requires: bv_popcount64
function bv_count_both(bv) {
    var n = 0;
    for (var i = 0; i < bv.nwords; i++) {
        n += bv_popcount64(bv.words[i * 2] & bv.words[i * 2 + 1]);
    }
    return n;
}

//Provides: bv_equal
function bv_equal(a, b) {
    if (a.nwords !== b.nwords) return 0;
    for (var i = 0; i < 2 * a.nwords; i++) {
        if (a.words[i] !== b.words[i]) {
            return 0;
        }
    }
    return 1;
}

//Provides: bv_init_from_list
function bv_init_from_list(bv, list) {
    var i = 0,
        pos = 0n,
        neg = 0n;
    // 0 = Val_emptylist in jsoo
    while (list !== 0) {
        // Field(list, 0): head
        var raw = list[1];
        // Field(list, 1): tail
        list = list[2];
        var bit = BigInt(i & 63);
        pos |= BigInt(raw & 1) << bit;
        neg |= BigInt((raw >> 1) & 1) << bit;
        if ((i & 63) === 63 || list === 0) {
            var word = i >> 6;
            bv.words[word * 2] = pos;
            bv.words[word * 2 + 1] = neg;
            pos = 0n;
            neg = 0n;
        }
        i++;
    }
    return 0;
}

//Provides: bv_init_from_array
function bv_init_from_array(bv, arr) {
    // Wosize_val: arr = [tag, v0, …]
    var width = arr.length - 1;
    var pos = 0n,
        neg = 0n;
    for (var i = 0; i < width; i++) {
        // Field(arr, i)
        var raw = arr[i + 1];
        var bit = BigInt(i & 63);
        pos |= BigInt(raw & 1) << bit;
        neg |= BigInt((raw >> 1) & 1) << bit;
        if ((i & 63) === 63 || i === width - 1) {
            var word = i >> 6;
            bv.words[word * 2] = pos;
            bv.words[word * 2 + 1] = neg;
            pos = 0n;
            neg = 0n;
        }
    }
    return 0;
}

//Provides: bv_to_array
function bv_to_array(bv, width) {
    // Atom(0): empty array block
    if (width === 0) return [0];
    var arr = new Array(width + 1);
    // block tag
    arr[0] = 0;
    var nw = bv.nwords;
    for (var i = 0; i < nw; i++) {
        var pos = bv.words[i * 2];
        var neg = bv.words[i * 2 + 1];
        var base = i << 6;
        var limit = width - base < 64 ? width - base : 64;
        for (var bit = 0; bit < limit; bit++) {
            var b = BigInt(bit);
            var raw = Number((pos >> b) & 1n) | (Number((neg >> b) & 1n) << 1);
            // Store_field(arr, base+bit, …)
            arr[base + bit + 1] = raw;
        }
    }
    return arr;
}

//Provides: bv_find_first
//Requires: bv_all_ones, bv_tail_mask, bv_ctz64
function bv_find_first(bv, width, raw) {
    var nw = bv.nwords;
    var want_pos = BigInt(raw & 1);
    var want_neg = BigInt((raw >> 1) & 1);
    for (var i = 0; i < nw; i++) {
        var m = i === nw - 1 ? bv_tail_mask(width) : bv_all_ones;
        var pos_word = bv.words[i * 2];
        var neg_word = bv.words[i * 2 + 1];
        var pos_match = want_pos ? pos_word : ~pos_word;
        var neg_match = want_neg ? neg_word : ~neg_word;
        var candidates = pos_match & neg_match & m;
        if (candidates !== 0n) {
            return (i << 6) + bv_ctz64(candidates);
        }
    }
    return -1;
}
