class BoundsException(Exception):
    def __init__(self, mi, ma):
        self.mi = mi
        self.ma = ma

    def __str__(self):
        return 'BoundsException: ({0}, {1})'.format(self.mi, self.ma)

# by default a closed interval, can be set to open
class Interval:
    def __init__(self, mi, ma, closed_left=True, closed_right=True):
        if mi > ma:
            raise BoundsExcpetion(mi, ma)
        else:
            self.mi = mi
            self.ma = ma
            self.closed_left = closed_left
            self.closed_right = closed_right
            self.width = ma - mi

    def __str__(self):
        lc = '('
        rc = ')'
        if self.closed_left:
            lc = '['
        if self.closed_right:
            rc = ']'
        return '{2}{0}, {1}{3}'.format(self.mi, self.ma, lc, rc)

    def __repr__(self):
        return self.__str__()

    def split(self):
        mid = (self.mi + self.ma) / 2
        left = Interval(self.mi, mid, closed_left=self.closed_left, closed_right=False)
        right = Interval(mid, self.ma, closed_left=True, closed_right=self.closed_right)
        return left, right

    def __add__(self, other):
        new_mi = self.mi + other.mi
        new_ma = self.ma + other.ma
        new_closed_left = self.closed_left and other.closed_left
        new_closed_right = self.closed_right and other.closed_right
        return Interval(new_mi, new_ma, closed_left=new_closed_left, closed_right=new_closed_right)

    def __iadd__(self, other):
        new_mi = self.mi + other.mi
        new_ma = self.ma + other.ma
        new_closed_left = self.closed_left and other.closed_left
        new_closed_right = self.closed_right and other.closed_right
        self.mi = new_mi
        self.ma = new_ma
        self.closed_left = new_closed_left
        self.closed_right = new_closed_right
        return self

    def __sub__(self, other):
        new_mi = self.mi - other.ma
        new_ma = self.ma - other.mi
        new_closed_left = self.closed_left and other.closed_right
        new_closed_right = self.closed_right and other.closed_left
        return Interval(new_mi, new_ma, closed_left=new_closed_left, closed_right=new_closed_right)

    def __isub__(self, other):
        new_mi = self.mi - other.ma
        new_ma = self.ma - other.mi
        new_closed_left = self.closed_left and other.closed_right
        new_closed_right = self.closed_right and other.closed_left
        self.mi = new_mi
        self.ma = new_ma
        self.closed_left = new_closed_left
        self.closed_right = new_closed_right
        return self

    # for all x in self: for all y in other: x < y
    def __lt__(self, other):
        if self.ma < other.mi:
            return True
        if self.ma == other.mi and (not self.closed_right or not other.closed_left):
            return True
        else:
            return False
    # for all x in self: for all y in other: x > y
    def __gt__(self, other):
        return other.__lt__(self)

    def __contains__(self, num):
        if num > self.ma or num < self.mi:
            return False
        if num == self.mi and self.closed_left:
            return True
        if num == self.ma and self.closed_right:
            return True
        if num > self.mi and num < self.ma:
            return True
        else:
            return False

# MAINTAIN DISJOINTITUDE

class IntervalMap:
    def __init__(self):
        self.pairs = []

    def add(self, interval, value):
        self.pairs.insert(0, (interval, value))
        return self
    
    def get(self, index):
        for piece, value in self.pairs:
            if index in piece:
                return value
        return None

class Piecewise:
    def __init__(self, pairs):
        self.imap = IntervalMap()
        for interval, rule in pairs:
            self.imap.add(interval, rule)
    
    def __call__(self, x):
        rule = self.imap.get(x)
        if rule == None:
            return None
        else:
            return rule(x)
