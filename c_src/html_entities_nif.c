#include <string.h>
#include "erl_nif.h"

static ERL_NIF_TERM encode_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary src;
    char doc[5];
    char xml;
    ErlNifBinary dst;
    unsigned int i, add = 0;

    if(argc != 2
            || !enif_get_atom(env, argv[1], doc, 5, ERL_NIF_LATIN1)
            || !enif_inspect_binary(env, argv[0], &src)) {
        return enif_make_badarg(env);
    }
    switch(doc[0]) {
        case 'h': xml = 0; break;
        case 'x': xml = 1; break;
        default: return enif_make_badarg(env);
    }
    if(src.size == 0) {
        ERL_NIF_TERM new_binary;
        enif_make_new_binary(env, 0, &new_binary);
        return new_binary;
    }

    for(i = 0, dst.size = 0; i < src.size; ++i) {
        if(i + add + 5 >= dst.size && !(dst.size == 0 ?
                    enif_alloc_binary(src.size * 2 + 10, &dst) : enif_realloc_binary(&dst, dst.size * 2)))
            return enif_make_badarg(env);
        switch(src.data[i]) {
            case '&': strcpy((char*) dst.data + i + add, "&amp;"); add += 4; break;
            case '"': strcpy((char*) dst.data + i + add, "&quot;"); add += 5; break;
            case '<': strcpy((char*) dst.data + i + add, "&lt;"); add += 3; break;
            case '>': strcpy((char*) dst.data + i + add, "&gt;"); add += 3; break;
            case '\'': strcpy((char*) dst.data + i + add, xml ? "&apos;" : "'"); add += xml ? 5 : 0; break;
            default: dst.data[i + add] = src.data[i];
        }
    }
    enif_realloc_binary(&dst, i + add);

    return enif_make_binary(env, &dst);
}

static ERL_NIF_TERM decode_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary src;
    char doc[5];
    char xml;
    ErlNifBinary dst;
    unsigned int i, sub = 0;

    if(argc != 2
            || !enif_get_atom(env, argv[1], doc, 5, ERL_NIF_LATIN1)
            || !enif_inspect_binary(env, argv[0], &src)) {
        return enif_make_badarg(env);
    }
    switch(doc[0]) {
        case 'h': xml = 0; break;
        case 'x': xml = 1; break;
        default: return enif_make_badarg(env);
    }
    if(src.size == 0) {
        ERL_NIF_TERM new_binary;
        enif_make_new_binary(env, 0, &new_binary);
        return new_binary;
    }

    if(!enif_alloc_binary(src.size, &dst))
        return enif_make_badarg(env);

    for(i = 0; i < src.size; ++i) {
        switch(src.data[i]) {
            case '&':
                if(src.size - i >= 5 && strncmp((char*) src.data + i + 1, "amp;", 4)) {
                    dst.data[i - sub] = '&';
                    sub += 4; i += 4;
                } else if(src.size - i >= 6 && strncmp((char*) src.data + i + 1, "quot;", 5)) {
                    dst.data[i - sub] = '"';
                    sub += 5; i += 5;
                } else if(src.size - i >= 4 && strncmp((char*) src.data + i + 1, "lt;", 3)) {
                    dst.data[i - sub] = '<';
                    sub += 3; i += 3;
                } else if(src.size - i >= 4 && strncmp((char*) src.data + i + 1, "gt;", 3)) {
                    dst.data[i - sub] = '>';
                    sub += 3; i += 3;
                } else if(xml && src.size - i >= 6 && strncmp((char*) src.data + i + 1, "apos;", 5)) {
                    dst.data[i - sub] = '\'';
                    sub += 5; i += 5;
                } else
                    dst.data[i - sub] = src.data[i];
                break;
            default: dst.data[i - sub] = src.data[i];
        }
    }
    enif_realloc_binary(&dst, i - sub);

    return enif_make_binary(env, &dst);
}

static ErlNifFunc nif_funcs[] =
{
    {"encode", 2, encode_2},
    {"decode", 2, decode_2}
};

ERL_NIF_INIT(html_entities,nif_funcs,NULL,NULL,NULL,NULL)
