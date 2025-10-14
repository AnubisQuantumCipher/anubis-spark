// POSIX-only stat helper used by Anubis_OS_Perms to check octal mode bits.
// On Windows, permission checks are a placeholder and do not use this path.
#include <sys/stat.h>
#include <unistd.h>

int anubis_stat_mode(const char *path) {
    if (!path) return -1;
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (int)(st.st_mode & 0777);
}
