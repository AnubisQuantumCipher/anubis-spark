#include <sys/stat.h>
#include <unistd.h>

int anubis_stat_mode(const char *path) {
    if (!path) return -1;
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (int)(st.st_mode & 0777);
}

