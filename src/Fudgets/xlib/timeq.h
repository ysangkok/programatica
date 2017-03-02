extern void inittimeq();

/* Make a time q entry */
extern struct timeq* mktimeq PROTO ((int, int, int));

extern unsigned long mstime PROTO ((void));

/* insert e in the time q */
/* value should be the offset from q start */
extern void entertimeq PROTO ((struct timeq *e));

/* remove the head element in the time q, reinsert it if it has an interval */
/* return copy of the timeq element (to get msg) */
extern struct timeq removetimeq PROTO ((void));

extern void cleartimeq PROTO ((int m));

struct timeq {
    struct timeq *next;		/* next element in q */
    int msg;			/* message to send */
    int value, interval;	/* expiration time (relative to timeqset), interval to next (or 0) */
};

#define TIMETOMS(tp) ((tp)->tv_sec * 1000L + (tp)->tv_usec / 1000)

extern unsigned long timeqset;		/* absolute time of q header */
extern struct timeq *timeq;	/* time q header */

#define QISEMPTY() (!timeq)

#define QTIME() (timeqset+timeq->value)
