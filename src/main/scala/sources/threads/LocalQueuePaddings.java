package sources.threads;

import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

class ClassPadding {

    private final int p00 = 0;

}

class HeadPadding extends ClassPadding {
    private final long p01 = 0;
    private final long p02 = 0;
    private final long p03 = 0;
    private final long p04 = 0;
    private final long p05 = 0;
    private final long p06 = 0;
    private final long p07 = 0;
    private final long p08 = 0;
    private final long p09 = 0;
    private final long p10 = 0;
    private final long p11 = 0;
    private final long p12 = 0;
    private final long p13 = 0;
    private final long p14 = 0;
    private final long p15 = 0;
    private final long p16 = 0;
}


class Head extends HeadPadding{

    protected static final AtomicIntegerFieldUpdater<Head> updater =
             AtomicIntegerFieldUpdater.newUpdater(Head.class,"head");

    private final int p00 = 0 ;

    private volatile int head;

}


class TailPadding extends Head{
    private final long p01 = 0;
    private final long p02 = 0;
    private final long p03 = 0;
    private final long p04 = 0;
    private final long p05 = 0;
    private final long p06 = 0;
    private final long p07 = 0;
    private final long p08 = 0;
    private final long p09 = 0;
    private final long p10 = 0;
    private final long p11 = 0;
    private final long p12 = 0;
    private final long p13 = 0;
    private final long p14 = 0;
    private final long p15 = 0;
    private final long p16 = 0;

}


class Tail extends TailPadding {

    protected static final AtomicIntegerFieldUpdater<Tail> updater  =
            AtomicIntegerFieldUpdater.newUpdater(Tail.class,"tailPublisher");

    protected int tail = 0 ;

    private volatile int tailPublisher ;

}

class LocalQueuePadding extends Tail {
    private final long p01 = 0;
    private final long p02 = 0;
    private final long p03 = 0;
    private final long p04 = 0;
    private final long p05 = 0;
    private final long p06 = 0;
    private final long p07 = 0;
    private final long p08 = 0;
    private final long p09 = 0;
    private final long p10 = 0;
    private final long p11 = 0;
    private final long p12 = 0;
    private final long p13 = 0;
    private final long p14 = 0;
    private final long p15 = 0;
    private final long p16 = 0;
}