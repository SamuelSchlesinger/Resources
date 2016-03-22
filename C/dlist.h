#ifndef __DLIST__
#define __DLIST__

typedef struct dlink *DLink;

struct dlink {
    DLink back;
    void* dum;
    DLink front;
};

typedef struct dlist *DList;

struct dlist {
    DLink head;
    int length;
    DLink tail;
};

DList newDList();

#endif
