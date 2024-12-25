import json
import numpy as np
from collections import namedtuple
import torch

# taken from https://stackoverflow.com/a/54577313
class CompactJSONEncoder(json.JSONEncoder):
    """A JSON Encoder that puts small lists on single lines."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.indentation_level = 0

    def encode(self, o):
        """Encode JSON object *o* with respect to single line lists."""
        if isinstance(o, (list, tuple)):
            if self._is_single_line_list(o):
                return "[" + ", ".join(json.dumps(el) for el in o) + "]"
            else:
                self.indentation_level += 1
                output = [self.indent_str + self.encode(el) for el in o]
                self.indentation_level -= 1
                return "[\n" + ",\n".join(output) + "\n" + self.indent_str + "]"

        elif isinstance(o, dict):
            self.indentation_level += 1
            output = [self.indent_str + f"{json.dumps(k)}: {self.encode(v)}" for k, v in o.items()]
            self.indentation_level -= 1
            return "{\n" + ",\n".join(output) + "\n" + self.indent_str + "}"

        else:
            return json.dumps(o)

    def _is_single_line_list(self, o):
        if isinstance(o, (list, tuple)):
            return not any(isinstance(el, (list, tuple, dict)) for el in o)\
                   and len(o) <= 2\
                   and len(str(o)) - 2 <= 60

    @property
    def indent_str(self) -> str:
        return " " * self.indentation_level * self.indent

    def iterencode(self, o, **kwargs):
        """Required to also work with `json.dump`."""
        return self.encode(o)

def compact_dump(obj, out_file, *args, **kwargs):
    out_file = open(out_file, "w")
    json.dump(obj, out_file, *args, **kwargs, indent = 2, cls=CompactJSONEncoder)
    out_file.close()


def as_json(*columns, col_names: list[str], outname:str, batch_size:int=10, outext:str="data.json", seq_lens=None):
    nrows = len(columns[0])
    assert nrows % batch_size == 0, f"num sequences must be divisible by batch size, got #rows: {nrows} and batch size {batch_size}"
    assert len(columns) == len(col_names), f"a column name must be assigned for each of the {len(columns)} columns. Got: {col_names}"

    nbatches = int(len(columns[0]) / batch_size)

    Point = namedtuple('Point', col_names)

    def tolist(xs):
        if isinstance(xs, np.ndarray):
            return xs.tolist()
        elif isinstance(xs, torch.Tensor):
            xsq = xs.squeeze().numpy()
            return xsq.tolist() if len(xsq.shape) > 1 else [xsq.tolist()]
        elif isinstance(list, xs):
            return xs
        else:
            raise Exception(f"unexpected data type: {type(xs)}")

    if seq_lens is None:
        def slice_data (i):
            slice = [tolist(col[i:i+nbatches]) for col in columns]
            return Point(*slice)

        batches = [slice_data(i) for i in range(0, nrows, int(nbatches))]
    else:
        def slice_data (i):
            slice = []
            for col in columns:
                xs = tolist(col[i:i+nbatches])
                truncated = xs if not isinstance(xs[0], list) else [c[:l] for (l, c) in zip(seq_lens[i:i+nbatches], xs)]
                slice.append(truncated)
            return Point(*slice)

        batches = [slice_data(i) for i in range(0, nrows, int(nbatches))]

    def mk_field(i):
        return {f"{n}{i+1}": getattr(batches[i], n) for n in col_names}

    final = {k: v for i in range(batch_size) for k, v in mk_field(i).items()}

    compact_dump(final, f"{outname}.{outext}")

    return final.keys()
