#include <iostream>
#include "fs.h"
#include "fs-ext.h"

using dump_c64 = StaticDumpable<char[64]>;
Bfsp bf("beef.data");

int main()
{
    {
        dump_c64 x, y;
        Dumpable *p = &x, *q = &y;
        strcpy(x.value, "Hello");
        p->Dump(bf, 0);
        q->Read(bf, 0);
        printf("%s\n", y.value);
    }

    {
        DumpableString s{"Hello"}, t{""};
        s.Dump(bf, 0);
        t.Read(bf, 0);
        printf("%s\n", t.value.value.data());
    }

    {
        DumpableVector<StaticDumpable<int>> vec{1, 2, 3};
    }
    return 0;
}