"""

Deterministic Finite Automata
-----------------------------
Very general, add a transition with += and add
frozensets, lists, or hashables to the accept
list with *=. The following example recognizes
proper binary addition.


d = DFA()

d.start = "start"
d *= ["good", "start"]

d += ("start", ('0', '1', '1'), "good")
d += ("start", ('1', '0', '1'), "good")
d += ("start", ('1', '1', '0'), "carry")
d += ("start", ('0', '0', '0'), "good")

d += ("carry", ('1', '0', '0'), "carry")
d += ("carry", ('0', '1', '0'), "carry")
d += ("carry", ('0', '0', '1'), "good")
d += ("carry", ('1', '1', '1'), "carry")

d += ("good", ('1', '0', '1'), "good")
d += ("good", ('0', '1', '1'), "good")
d += ("good", ('1', '1', '0'), "carry")
d += ("good", ('0', '0', '0'), "good")

"""

class DFA:
    def __init__(self):
        self.transition = {}
        self.accept = frozenset([])
        self.start = 1

    def __iadd__(self, (src, c, dest)):
        if src in self.transition:
            self.transition[src][c] = dest    
        else:
            self.transition[src] = { c : dest }
        return self

    def add_accept(self, state):
        if isinstance(state, frozenset):
            self.accept |= states 
        elif isinstance(state, list):
            self.accept |= frozenset(state)
        else:
            self.accept |= frozenset([state])
        return self

    def __imul__(self, state):
        return self.add_accept(state)

    def __call__(self, pattern):
        state = self.start
        for c in pattern:
            if state in self.transition:
                if c in self.transition[state]:
                    state = self.transition[state][c]
                else:
                    return False
            else:
                return False
        return state in self.accept
