/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;
#include "HsBase.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_s_issock
#endif

static void hugsprim___hscore_s_issock_78(HugsStackPtr);
static void hugsprim___hscore_s_issock_78(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsBool res1;
    arg1 = hugs->getWord32();
    res1 = __hscore_s_issock(arg1);
    hugs->putBool(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_ptr_c_cc
#endif

static void hugsprim___hscore_ptr_c_cc_77(HugsStackPtr);
static void hugsprim___hscore_ptr_c_cc_77(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_ptr_c_cc(arg1);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_poke_lflag
#endif

static void hugsprim___hscore_poke_lflag_76(HugsStackPtr);
static void hugsprim___hscore_poke_lflag_76(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 arg2;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord32();
    __hscore_poke_lflag(arg1, arg2);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_lflag
#endif

static void hugsprim___hscore_lflag_75(HugsStackPtr);
static void hugsprim___hscore_lflag_75(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_lflag(arg1);
    hugs->putWord32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sizeof_sigset_t
#endif

static void hugsprim___hscore_sizeof_sigset_t_74(HugsStackPtr);
static void hugsprim___hscore_sizeof_sigset_t_74(HugsStackPtr hugs_root)
{
    HsInt res1;
    res1 = __hscore_sizeof_sigset_t();
    hugs->putInt(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sizeof_termios
#endif

static void hugsprim___hscore_sizeof_termios_73(HugsStackPtr);
static void hugsprim___hscore_sizeof_termios_73(HugsStackPtr hugs_root)
{
    HsInt res1;
    res1 = __hscore_sizeof_termios();
    hugs->putInt(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_f_setfl
#endif

static void hugsprim___hscore_f_setfl_72(HugsStackPtr);
static void hugsprim___hscore_f_setfl_72(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_f_setfl();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_f_getfl
#endif

static void hugsprim___hscore_f_getfl_71(HugsStackPtr);
static void hugsprim___hscore_f_getfl_71(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_f_getfl();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sig_setmask
#endif

static void hugsprim___hscore_sig_setmask_70(HugsStackPtr);
static void hugsprim___hscore_sig_setmask_70(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_sig_setmask();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sig_block
#endif

static void hugsprim___hscore_sig_block_69(HugsStackPtr);
static void hugsprim___hscore_sig_block_69(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_sig_block();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sigttou
#endif

static void hugsprim___hscore_sigttou_68(HugsStackPtr);
static void hugsprim___hscore_sigttou_68(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_sigttou();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_vtime
#endif

static void hugsprim___hscore_vtime_67(HugsStackPtr);
static void hugsprim___hscore_vtime_67(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_vtime();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_vmin
#endif

static void hugsprim___hscore_vmin_66(HugsStackPtr);
static void hugsprim___hscore_vmin_66(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_vmin();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_icanon
#endif

static void hugsprim___hscore_icanon_65(HugsStackPtr);
static void hugsprim___hscore_icanon_65(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_icanon();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_tcsanow
#endif

static void hugsprim___hscore_tcsanow_64(HugsStackPtr);
static void hugsprim___hscore_tcsanow_64(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_tcsanow();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_echo
#endif

static void hugsprim___hscore_echo_63(HugsStackPtr);
static void hugsprim___hscore_echo_63(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_echo();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_st_mode
#endif

static void hugsprim___hscore_st_mode_62(HugsStackPtr);
static void hugsprim___hscore_st_mode_62(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_st_mode(arg1);
    hugs->putWord32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_st_size
#endif

static void hugsprim___hscore_st_size_61(HugsStackPtr);
static void hugsprim___hscore_st_size_61(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt64 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_st_size(arg1);
    hugs->putInt64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_st_mtime
#endif

static void hugsprim___hscore_st_mtime_60(HugsStackPtr);
static void hugsprim___hscore_st_mtime_60(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt64 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_st_mtime(arg1);
    hugs->putInt64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sizeof_stat
#endif

static void hugsprim___hscore_sizeof_stat_59(HugsStackPtr);
static void hugsprim___hscore_sizeof_stat_59(HugsStackPtr hugs_root)
{
    HsInt res1;
    res1 = __hscore_sizeof_stat();
    hugs->putInt(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_s_isfifo
#endif

static void hugsprim___hscore_s_isfifo_58(HugsStackPtr);
static void hugsprim___hscore_s_isfifo_58(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsBool res1;
    arg1 = hugs->getWord32();
    res1 = __hscore_s_isfifo(arg1);
    hugs->putBool(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_s_isdir
#endif

static void hugsprim___hscore_s_isdir_57(HugsStackPtr);
static void hugsprim___hscore_s_isdir_57(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsBool res1;
    arg1 = hugs->getWord32();
    res1 = __hscore_s_isdir(arg1);
    hugs->putBool(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_s_isblk
#endif

static void hugsprim___hscore_s_isblk_56(HugsStackPtr);
static void hugsprim___hscore_s_isblk_56(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsBool res1;
    arg1 = hugs->getWord32();
    res1 = __hscore_s_isblk(arg1);
    hugs->putBool(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_s_ischr
#endif

static void hugsprim___hscore_s_ischr_55(HugsStackPtr);
static void hugsprim___hscore_s_ischr_55(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsBool res1;
    arg1 = hugs->getWord32();
    res1 = __hscore_s_ischr(arg1);
    hugs->putBool(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_s_isreg
#endif

static void hugsprim___hscore_s_isreg_54(HugsStackPtr);
static void hugsprim___hscore_s_isreg_54(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsBool res1;
    arg1 = hugs->getWord32();
    res1 = __hscore_s_isreg(arg1);
    hugs->putBool(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_binary
#endif

static void hugsprim___hscore_o_binary_53(HugsStackPtr);
static void hugsprim___hscore_o_binary_53(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_binary();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_nonblock
#endif

static void hugsprim___hscore_o_nonblock_52(HugsStackPtr);
static void hugsprim___hscore_o_nonblock_52(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_nonblock();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_noctty
#endif

static void hugsprim___hscore_o_noctty_51(HugsStackPtr);
static void hugsprim___hscore_o_noctty_51(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_noctty();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_trunc
#endif

static void hugsprim___hscore_o_trunc_50(HugsStackPtr);
static void hugsprim___hscore_o_trunc_50(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_trunc();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_excl
#endif

static void hugsprim___hscore_o_excl_49(HugsStackPtr);
static void hugsprim___hscore_o_excl_49(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_excl();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_creat
#endif

static void hugsprim___hscore_o_creat_48(HugsStackPtr);
static void hugsprim___hscore_o_creat_48(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_creat();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_append
#endif

static void hugsprim___hscore_o_append_47(HugsStackPtr);
static void hugsprim___hscore_o_append_47(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_append();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_rdwr
#endif

static void hugsprim___hscore_o_rdwr_46(HugsStackPtr);
static void hugsprim___hscore_o_rdwr_46(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_rdwr();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_wronly
#endif

static void hugsprim___hscore_o_wronly_45(HugsStackPtr);
static void hugsprim___hscore_o_wronly_45(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_wronly();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_o_rdonly
#endif

static void hugsprim___hscore_o_rdonly_44(HugsStackPtr);
static void hugsprim___hscore_o_rdonly_44(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_o_rdonly();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_d_name
#endif

static void hugsprim___hscore_d_name_43(HugsStackPtr);
static void hugsprim___hscore_d_name_43(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_d_name(arg1);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_end_of_dir
#endif

static void hugsprim___hscore_end_of_dir_42(HugsStackPtr);
static void hugsprim___hscore_end_of_dir_42(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_end_of_dir();
    hugs->putInt32(res1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_free_dirent
#endif

static void hugsprim___hscore_free_dirent_41(HugsStackPtr);
static void hugsprim___hscore_free_dirent_41(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    arg1 = hugs->getPtr();
    __hscore_free_dirent(arg1);
    
    hugs->returnIO(hugs_root,0);
}
#include "dirUtils.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_readdir
#endif

static void hugsprim___hscore_readdir_40(HugsStackPtr);
static void hugsprim___hscore_readdir_40(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    res1 = __hscore_readdir(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef waitpid
#endif

static void hugsprim_waitpid_39(HugsStackPtr);
static void hugsprim_waitpid_39(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr arg2;
    HsInt32 arg3;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getPtr();
    arg3 = hugs->getInt32();
    res1 = waitpid(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef utime
#endif

static void hugsprim_utime_38(HugsStackPtr);
static void hugsprim_utime_38(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsWord32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    res1 = utime(arg1, arg2);
    hugs->putWord32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef tcsetattr
#endif

static void hugsprim_tcsetattr_37(HugsStackPtr);
static void hugsprim_tcsetattr_37(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsPtr arg3;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    arg3 = hugs->getPtr();
    res1 = tcsetattr(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef tcgetattr
#endif

static void hugsprim_tcgetattr_36(HugsStackPtr);
static void hugsprim_tcgetattr_36(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getPtr();
    res1 = tcgetattr(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef sigprocmask
#endif

static void hugsprim_sigprocmask_35(HugsStackPtr);
static void hugsprim_sigprocmask_35(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr arg2;
    HsPtr arg3;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getPtr();
    arg3 = hugs->getPtr();
    res1 = sigprocmask(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sigaddset
#endif

static void hugsprim___hscore_sigaddset_34(HugsStackPtr);
static void hugsprim___hscore_sigaddset_34(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt32();
    res1 = __hscore_sigaddset(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_sigemptyset
#endif

static void hugsprim___hscore_sigemptyset_33(HugsStackPtr);
static void hugsprim___hscore_sigemptyset_33(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_sigemptyset(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef pipe
#endif

static void hugsprim_pipe_32(HugsStackPtr);
static void hugsprim_pipe_32(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = pipe(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef mkfifo
#endif

static void hugsprim_mkfifo_31(HugsStackPtr);
static void hugsprim_mkfifo_31(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord32();
    res1 = mkfifo(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef link
#endif

static void hugsprim_link_30(HugsStackPtr);
static void hugsprim_link_30(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    res1 = link(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fork
#endif

static void hugsprim_fork_29(HugsStackPtr);
static void hugsprim_fork_29(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = fork();
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fcntl
#endif

static void hugsprim_fcntl_28(HugsStackPtr);
static void hugsprim_fcntl_28(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsPtr arg3;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    arg3 = hugs->getPtr();
    res1 = fcntl(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fcntl
#endif

static void hugsprim_fcntl_27(HugsStackPtr);
static void hugsprim_fcntl_27(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsInt32 arg3;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    arg3 = hugs->getInt32();
    res1 = fcntl(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fcntl
#endif

static void hugsprim_fcntl_26(HugsStackPtr);
static void hugsprim_fcntl_26(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    res1 = fcntl(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef getpid
#endif

static void hugsprim_getpid_25(HugsStackPtr);
static void hugsprim_getpid_25(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = getpid();
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef unlink
#endif

static void hugsprim_unlink_24(HugsStackPtr);
static void hugsprim_unlink_24(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = unlink(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_ftruncate
#endif

static void hugsprim___hscore_ftruncate_23(HugsStackPtr);
static void hugsprim___hscore_ftruncate_23(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt64 arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt64();
    res1 = __hscore_ftruncate(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef write
#endif

static void hugsprim_write_22(HugsStackPtr);
static void hugsprim_write_22(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr arg2;
    HsWord64 arg3;
    HsInt64 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    res1 = write(arg1, arg2, arg3);
    hugs->putInt64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef umask
#endif

static void hugsprim_umask_21(HugsStackPtr);
static void hugsprim_umask_21(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsWord32 res1;
    arg1 = hugs->getWord32();
    res1 = umask(arg1);
    hugs->putWord32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_stat
#endif

static void hugsprim___hscore_stat_20(HugsStackPtr);
static void hugsprim___hscore_stat_20(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    res1 = __hscore_stat(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef rmdir
#endif

static void hugsprim_rmdir_19(HugsStackPtr);
static void hugsprim_rmdir_19(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = rmdir(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef rewinddir
#endif

static void hugsprim_rewinddir_18(HugsStackPtr);
static void hugsprim_rewinddir_18(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    arg1 = hugs->getPtr();
    rewinddir(arg1);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_renameFile
#endif

static void hugsprim___hscore_renameFile_17(HugsStackPtr);
static void hugsprim___hscore_renameFile_17(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    res1 = __hscore_renameFile(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef read
#endif

static void hugsprim_read_16(HugsStackPtr);
static void hugsprim_read_16(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr arg2;
    HsWord64 arg3;
    HsInt64 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    res1 = read(arg1, arg2, arg3);
    hugs->putInt64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_mkdir
#endif

static void hugsprim___hscore_mkdir_15(HugsStackPtr);
static void hugsprim___hscore_mkdir_15(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt32();
    res1 = __hscore_mkdir(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef opendir
#endif

static void hugsprim_opendir_14(HugsStackPtr);
static void hugsprim_opendir_14(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr res1;
    arg1 = hugs->getPtr();
    res1 = opendir(arg1);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_open
#endif

static void hugsprim___hscore_open_13(HugsStackPtr);
static void hugsprim___hscore_open_13(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 arg2;
    HsWord32 arg3;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt32();
    arg3 = hugs->getWord32();
    res1 = __hscore_open(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_lstat
#endif

static void hugsprim___hscore_lstat_12(HugsStackPtr);
static void hugsprim___hscore_lstat_12(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    res1 = __hscore_lstat(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_lseek
#endif

static void hugsprim___hscore_lseek_11(HugsStackPtr);
static void hugsprim___hscore_lseek_11(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt64 arg2;
    HsInt32 arg3;
    HsInt64 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt64();
    arg3 = hugs->getInt32();
    res1 = __hscore_lseek(arg1, arg2, arg3);
    hugs->putInt64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef isatty
#endif

static void hugsprim_isatty_10(HugsStackPtr);
static void hugsprim_isatty_10(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    res1 = isatty(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef getcwd
#endif

static void hugsprim_getcwd_9(HugsStackPtr);
static void hugsprim_getcwd_9(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 arg2;
    HsPtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt32();
    res1 = getcwd(arg1, arg2);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_fstat
#endif

static void hugsprim___hscore_fstat_8(HugsStackPtr);
static void hugsprim___hscore_fstat_8(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getPtr();
    res1 = __hscore_fstat(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef dup2
#endif

static void hugsprim_dup2_7(HugsStackPtr);
static void hugsprim_dup2_7(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    res1 = dup2(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef dup
#endif

static void hugsprim_dup_6(HugsStackPtr);
static void hugsprim_dup_6(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    res1 = dup(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef creat
#endif

static void hugsprim_creat_5(HugsStackPtr);
static void hugsprim_creat_5(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord32();
    res1 = creat(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef closedir
#endif

static void hugsprim_closedir_4(HugsStackPtr);
static void hugsprim_closedir_4(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = closedir(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef close
#endif

static void hugsprim_close_3(HugsStackPtr);
static void hugsprim_close_3(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    res1 = close(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef chdir
#endif

static void hugsprim_chdir_2(HugsStackPtr);
static void hugsprim_chdir_2(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = chdir(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef chmod
#endif

static void hugsprim_chmod_1(HugsStackPtr);
static void hugsprim_chmod_1(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord32();
    res1 = chmod(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef access
#endif

static void hugsprim_access_0(HugsStackPtr);
static void hugsprim_access_0(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord32();
    res1 = access(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"s_issock", 1, hugsprim___hscore_s_issock_78},
    {"ptr_c_cc", 2, hugsprim___hscore_ptr_c_cc_77},
    {"poke_c_lflag", 3, hugsprim___hscore_poke_lflag_76},
    {"c_lflag", 2, hugsprim___hscore_lflag_75},
    {"sizeof_sigset_t", 0, hugsprim___hscore_sizeof_sigset_t_74},
    {"sizeof_termios", 0, hugsprim___hscore_sizeof_termios_73},
    {"const_f_setfl", 0, hugsprim___hscore_f_setfl_72},
    {"const_f_getfl", 0, hugsprim___hscore_f_getfl_71},
    {"const_sig_setmask", 0, hugsprim___hscore_sig_setmask_70},
    {"const_sig_block", 0, hugsprim___hscore_sig_block_69},
    {"const_sigttou", 0, hugsprim___hscore_sigttou_68},
    {"const_vtime", 0, hugsprim___hscore_vtime_67},
    {"const_vmin", 0, hugsprim___hscore_vmin_66},
    {"const_icanon", 0, hugsprim___hscore_icanon_65},
    {"const_tcsanow", 0, hugsprim___hscore_tcsanow_64},
    {"const_echo", 0, hugsprim___hscore_echo_63},
    {"st_mode", 2, hugsprim___hscore_st_mode_62},
    {"st_size", 2, hugsprim___hscore_st_size_61},
    {"st_mtime", 2, hugsprim___hscore_st_mtime_60},
    {"sizeof_stat", 0, hugsprim___hscore_sizeof_stat_59},
    {"s_isfifo", 1, hugsprim___hscore_s_isfifo_58},
    {"s_isdir", 1, hugsprim___hscore_s_isdir_57},
    {"s_isblk", 1, hugsprim___hscore_s_isblk_56},
    {"s_ischr", 1, hugsprim___hscore_s_ischr_55},
    {"s_isreg", 1, hugsprim___hscore_s_isreg_54},
    {"o_BINARY", 0, hugsprim___hscore_o_binary_53},
    {"o_NONBLOCK", 0, hugsprim___hscore_o_nonblock_52},
    {"o_NOCTTY", 0, hugsprim___hscore_o_noctty_51},
    {"o_TRUNC", 0, hugsprim___hscore_o_trunc_50},
    {"o_EXCL", 0, hugsprim___hscore_o_excl_49},
    {"o_CREAT", 0, hugsprim___hscore_o_creat_48},
    {"o_APPEND", 0, hugsprim___hscore_o_append_47},
    {"o_RDWR", 0, hugsprim___hscore_o_rdwr_46},
    {"o_WRONLY", 0, hugsprim___hscore_o_wronly_45},
    {"o_RDONLY", 0, hugsprim___hscore_o_rdonly_44},
    {"d_name", 2, hugsprim___hscore_d_name_43},
    {"end_of_dir", 0, hugsprim___hscore_end_of_dir_42},
    {"freeDirEnt", 2, hugsprim___hscore_free_dirent_41},
    {"readdir", 3, hugsprim___hscore_readdir_40},
    {"c_waitpid", 4, hugsprim_waitpid_39},
    {"c_utime", 3, hugsprim_utime_38},
    {"c_tcsetattr", 4, hugsprim_tcsetattr_37},
    {"c_tcgetattr", 3, hugsprim_tcgetattr_36},
    {"c_sigprocmask", 4, hugsprim_sigprocmask_35},
    {"c_sigaddset", 3, hugsprim___hscore_sigaddset_34},
    {"c_sigemptyset", 2, hugsprim___hscore_sigemptyset_33},
    {"c_pipe", 2, hugsprim_pipe_32},
    {"c_mkfifo", 3, hugsprim_mkfifo_31},
    {"c_link", 3, hugsprim_link_30},
    {"c_fork", 1, hugsprim_fork_29},
    {"c_fcntl_lock", 4, hugsprim_fcntl_28},
    {"c_fcntl_write", 4, hugsprim_fcntl_27},
    {"c_fcntl_read", 3, hugsprim_fcntl_26},
    {"c_getpid", 1, hugsprim_getpid_25},
    {"c_unlink", 2, hugsprim_unlink_24},
    {"c_ftruncate", 3, hugsprim___hscore_ftruncate_23},
    {"c_write", 4, hugsprim_write_22},
    {"c_umask", 2, hugsprim_umask_21},
    {"c_stat", 3, hugsprim___hscore_stat_20},
    {"c_rmdir", 2, hugsprim_rmdir_19},
    {"c_rewinddir", 2, hugsprim_rewinddir_18},
    {"c_rename", 3, hugsprim___hscore_renameFile_17},
    {"c_read", 4, hugsprim_read_16},
    {"mkdir", 3, hugsprim___hscore_mkdir_15},
    {"c_opendir", 2, hugsprim_opendir_14},
    {"c_open", 4, hugsprim___hscore_open_13},
    {"lstat", 3, hugsprim___hscore_lstat_12},
    {"c_lseek", 4, hugsprim___hscore_lseek_11},
    {"c_isatty", 2, hugsprim_isatty_10},
    {"c_getcwd", 3, hugsprim_getcwd_9},
    {"c_fstat", 3, hugsprim___hscore_fstat_8},
    {"c_dup2", 3, hugsprim_dup2_7},
    {"c_dup", 2, hugsprim_dup_6},
    {"c_creat", 3, hugsprim_creat_5},
    {"c_closedir", 2, hugsprim_closedir_4},
    {"c_close", 2, hugsprim_close_3},
    {"c_chdir", 2, hugsprim_chdir_2},
    {"c_chmod", 3, hugsprim_chmod_1},
    {"c_access", 3, hugsprim_access_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initSPInternals
#endif

static struct hugs_primInfo hugs_prims = { hugs_primControl, hugs_primTable, 0 };

#ifdef __cplusplus
extern "C" {
#endif
#ifndef __cplusplus
DLLEXPORT(int)  HugsAPIVersion(void);
#endif
DLLEXPORT(int)  HugsAPIVersion() {return (5);}
DLLEXPORT(void) initModule(HugsAPI5 *);
DLLEXPORT(void) initModule(HugsAPI5 *hugsAPI) {
    hugs = hugsAPI;
    hugs->registerPrims(&hugs_prims);
}
#ifdef __cplusplus
}
#endif

