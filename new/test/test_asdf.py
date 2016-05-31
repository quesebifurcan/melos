from new.lib import (
    schemas,
    utils,
)

def test_assoc():
    a, b = (
        schemas.Note(pitch=0),
        schemas.Note(pitch=1),
    )
    b_ = utils.assoc(b, 'pitch', 0)
    assert a != b
    assert a == b_

