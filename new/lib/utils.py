from new.lib import schemas

def assoc(m, k, v):
    return m._replace(**{k: v})

def update(m, k, f):
    v = f(getattr(m, k))
    return m._replace(**{k: v})

def print_(node, indentation=0):
    inset = ' ' * indentation
    print('{}-----{}-----'.format(inset, type(node)))
    for k, v in node._asdict().items():
        if isinstance(v, (
                schemas.Note,
                schemas.Chord, schemas.RhythmTreeNode)):
            print('{}{}: '.format(inset, k))
            print_(v, indentation+2)
        elif isinstance(v, list):
            print('{}{}: ['.format(inset, k))
            for vv in v:
                print_(vv, indentation+2)
            print('{}]'.format(inset))
        else:
            print('{}{}: {}'.format(inset, k, v))


