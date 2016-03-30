"""

d = DFA()

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
