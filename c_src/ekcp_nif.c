#include "erl_nif.h"
#include "ikcp.h"
#include <string.h>

#define RET_OK \
    return enif_make_atom(env, "ok");

#define RET_BIN0(bin_term) { \
    enif_make_new_binary(env, 0, &bin_term); \
    return bin_term;}

#define RET_INT(var) \
    return enif_make_int(env, var);

#define RET_UINT(var) \
    return enif_make_uint(env, var);

#define RET_BAD \
    return enif_make_badarg(env);

#define GET_KCP \
    ikcpcb *kcp; \
    if (!enif_get_resource(env, argv[0], ekcp_recource_type, (void **)&kcp)) RET_BAD

#define GET_INT(index, var) \
    int var; \
    if (!enif_get_int(env, argv[index], &var)) RET_BAD

#define GET_UINT(index, var) \
    unsigned int var; \
    if (!enif_get_uint(env, argv[index], &var)) RET_BAD

#define GET_BIN(index, var) \
    ErlNifBinary var; \
    if (!enif_inspect_binary(env, argv[index], &var)) RET_BAD

#define GET_PID(index, var) \
    ErlNifPid var; \
    if (!enif_get_local_pid(env, argv[index], &var)) RET_BAD

typedef struct EkcpUser
{
    unsigned int conv;
    ErlNifPid udp_pid;
    ErlNifEnv *self_env;
} ekcp_user;

static ErlNifResourceType *ekcp_recource_type;

static void* ekcp_malloc(size_t size)
{
	return enif_alloc_resource(ekcp_recource_type, size);
}

static void ekcp_free(void *ptr)
{
    enif_release_resource(ptr);
}

static ERL_NIF_TERM load_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ekcp_recource_type = enif_open_resource_type(env, NULL, "ekcp_recource_type", NULL, ERL_NIF_RT_CREATE, NULL);
    if (!ekcp_recource_type) RET_BAD
    ikcp_allocator(ekcp_malloc, ekcp_free);
    RET_OK
}

static int output(const char *buf, int len, ikcpcb *kcp, void *user)
{
    ERL_NIF_TERM bin;
    ekcp_user *eu = (ekcp_user*)user;
    ErlNifEnv *msg_env = enif_alloc_env();
    char *p = (char *)enif_make_new_binary(msg_env, len, &bin);
    memcpy(p, buf, len);

    ERL_NIF_TERM msg = enif_make_tuple2(msg_env, enif_make_atom(msg_env, "ekcp_output"), bin);

    int ret = enif_send(eu->self_env, &eu->udp_pid, msg_env, msg);
    enif_free_env(msg_env);
    return ret;
}

//---------------------------------------------------------------------
// ekcp interfaces
//---------------------------------------------------------------------
static ERL_NIF_TERM ekcp_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_UINT(0, conv)
    GET_PID(1, udp_pid)

    ekcp_user *user = (ekcp_user *)ekcp_malloc(sizeof(ekcp_user));
    user->conv = conv;
    user->self_env = env;
    user->udp_pid = udp_pid;

    ikcpcb *kcp = ikcp_create(conv, user);
    ikcp_setoutput(kcp, output);
    return enif_make_resource(env, kcp);
}

static ERL_NIF_TERM ekcp_release(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    ekcp_free(kcp->user);
    ikcp_release(kcp);
    RET_OK
}

static ERL_NIF_TERM ekcp_recv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    ERL_NIF_TERM bin_term;
    int len = ikcp_peeksize(kcp);
    if (len <= 0) RET_BIN0(bin_term)
    unsigned char *buffer = enif_make_new_binary(env, len, &bin_term);
    if (buffer == NULL) RET_BIN0(bin_term)
    int size = ikcp_recv(kcp, (char *)buffer, len);
    if (size < 0) RET_BIN0(bin_term)
    return bin_term;
}

static ERL_NIF_TERM ekcp_send(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_BIN(1, bin)
    RET_INT(ikcp_send(kcp, (char *)bin.data, bin.size))
}

static ERL_NIF_TERM ekcp_update(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_UINT(1, current)
    ikcp_update(kcp, current);
    RET_OK
}

static ERL_NIF_TERM ekcp_check(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_UINT(1, current)
    RET_UINT(ikcp_check(kcp, current))
}

static ERL_NIF_TERM ekcp_input(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_BIN(1, bin)
    RET_INT(ikcp_input(kcp, (char *)bin.data, bin.size))
}

static ERL_NIF_TERM ekcp_flush(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    ikcp_flush(kcp);
    RET_OK
}

static ERL_NIF_TERM ekcp_peeksize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    RET_INT(ikcp_peeksize(kcp))
}

static ERL_NIF_TERM ekcp_setmtu(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_INT(1, mtu)
    RET_INT(ikcp_setmtu(kcp, mtu))
}

static ERL_NIF_TERM ekcp_wndsize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_INT(1, sndwnd)
    GET_INT(2, rcvwnd)
    RET_INT(ikcp_wndsize(kcp, sndwnd, rcvwnd))
}

static ERL_NIF_TERM ekcp_waitsnd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    RET_INT(ikcp_waitsnd(kcp))
}

static ERL_NIF_TERM ekcp_nodelay(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_KCP
    GET_INT(1, nodelay)
    GET_INT(2, interval)
    GET_INT(3, resend)
    GET_INT(4, nc)
    RET_INT(ikcp_nodelay(kcp, nodelay, interval, resend, nc))
}

static ERL_NIF_TERM ekcp_getconv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    GET_BIN(0, bin)
    RET_UINT(ikcp_getconv(bin.data))
}


static ErlNifFunc nif_funcs[] = {
    {"load_init", 0, load_init},
    {"create", 2, ekcp_create},
    {"release", 1, ekcp_release},
    {"recv", 1, ekcp_recv},
    {"send", 2, ekcp_send},
    {"update", 2, ekcp_update},
    {"check", 2, ekcp_check},
    {"input", 2, ekcp_input},
    {"flush", 1, ekcp_flush},
    {"peeksize", 1, ekcp_peeksize},
    {"setmtu", 2, ekcp_setmtu},
    {"wndsize", 3, ekcp_wndsize},
    {"waitsnd", 1, ekcp_waitsnd},
    {"nodelay", 5, ekcp_nodelay},
    {"getconv", 1, ekcp_getconv}};

ERL_NIF_INIT(ekcp, nif_funcs, NULL, NULL, NULL, NULL)
