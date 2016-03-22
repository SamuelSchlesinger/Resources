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

/*
Constructs a new DList of length 0
*/
DList newDList();

/*
Returns the length of the DList
*/
int length_d(DList d);


/*
Pushes a new DLink with the given void* to the front and back respectively
*/
void push_front_d(DList, void*);

void push_back_d(DList, void*);


/*
Gets the front or back or nth element, ordered with the head as 0
*/
void* get_front_d(DList);

void* get_back_d(DList);

void* get_nth_d(DList, int); // n >= 0

/*
Gets and deletes the front or back or nth element similarly to above
*/
void pop_front_d(DList);

void pop_back_d(DList);

void pop_nth_d(DList);

#endif
