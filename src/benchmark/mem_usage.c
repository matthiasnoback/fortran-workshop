#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#else
#include <sys/resource.h>
#endif

int get_memory_usage_kb() {
#ifdef _WIN32
    PROCESS_MEMORY_COUNTERS pmc;
    GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc));
    return (int)(pmc.WorkingSetSize / 1024);  // in KB
#else
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    return (int)(usage.ru_maxrss);  // in KB
#endif
}
