#include <sys/types.h>
#include <signal.h>
#include <errno.h>

int killpg(pid_t pgid, int sig)
{
    if (pgid == 1) {
	errno = ESRCH;
        return (-1);
    }
    return (kill(-pgid, sig));
}
