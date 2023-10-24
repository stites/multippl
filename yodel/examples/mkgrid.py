import numpy as np

def exact_root(n, p):
    return f"let x{n} = flip {p} in"

def exact_edge(n, parent, t, f):
    return f"let x{n} = if {parent} then flip {t} else flip {f} in"

def sample_edge(n, parent, t, f):
    return f"x{n} ~ if {parent} then bern({t}) else bern({f});"

def sample_node(n, p0, p1, tt, tf, ft, ff):
    return '''\
x{n} ~ if  {p0} &&  {p1} then bern({tt}) else
          if  {p0} && !{p1} then bern({tf}) else
          if !{p0} &&  {p1} then bern({ft}) else
                               bern({ff});
'''.format(n=n, p0=p0, p1=p1, tt=tt, tf=tf, ft=ft, ff=ff)

def exact_node(n, p0, p1, tt, tf, ft, ff):
    return '''\
let x{n} =
    if  {p0} &&  {p1} then flip {tt} else
    if  {p0} && !{p1} then flip {tf} else
    if !{p0} &&  {p1} then flip {ft} else
                           flip {ff} in
'''.format(n=n, p0=p0, p1=p1, tt=tt, tf=tf, ft=ft, ff=ff)

def parentnames(g, i, j):
   match (i, j):
       case (0, 0):
           return []
       case (0, _):
           return ["x0"+str(j-1)]
       case (_, 0):
           return ["x"+str(i-1)+"0"]
       case _:
           return ["x"+str(i)+str(j-1), "x"+str(i-1)+str(j)]

def mkgrid(n, parents, probfn, returnfn, sample=False):
    def mk(i, j, *args):
       match (i, j):
           case (0, 0):
               return exact_root("00", *args)
           case (0, _):
               return exact_edge("0"+str(j), *args)
           case (_, 0):
               return exact_edge(str(i)+"0", *args)
           case _:
               return exact_node(str(i)+str(j), *args)

    def mks(i, j, *args):
       match (i, j):
           case (0, _):
               return sample_edge("0"+str(j), *args)
           case (_, 0):
               return sample_edge(str(i)+"0", *args)
           case _:
               return sample_node(str(i)+str(j), *args)


    grid = []
    for i in range(0,n):
        grid.append([])
        for j in range(0,n):
            if sample and i + j == (n-1):
                grid[i].append(mks(i, j, *parents(grid, i, j), *probfn(i, j)))
            else:
                grid[i].append(mk(i, j, *parents(grid, i, j), *probfn(i, j)))
    return returnfn(grid, n, sample)

def returnfn(g, n, sample=False):
    program = "exact {\n"
    (triu, diag, tril) = reorder(n)
    for row in triu:
        for (i,j) in row:
            program += "  " + g[i][j] + "\n"
    diag_indent = '  '
    if sample:
        program += "  let diag = sample {\n"
        diag_indent += '  '
    for (i,j) in diag:
        program += diag_indent + g[i][j] + "\n"
    if sample:
        program += "    (" + ", ".join(list(map(lambda ij: f"x{ij[0]}{ij[1]}",  diag))) + ")\n"
        program += "  } in\n"
        for ix, (i, j) in enumerate(diag):
            program += f"  let x{i}{j} = diag[{ix}] in\n"

    for row in tril:
        for (i,j) in row:
            program += "  " + g[i][j] + "\n"

    query = []
    for i, row in enumerate(g):
        for j, cell in enumerate(row):
            query.append(f"x{i}{j}")
    program += "  (" + ", ".join(query) + ")\n"
    program += "}"
    return program

def reorder(n):
    m = np.matrix(np.zeros((n,n)).astype(int))
    ixs_i = m + np.reshape(np.arange(0, n), (n,1))
    dia_i = ixs_i.diagonal()[:,::-1]
    triu_i = np.fliplr(np.triu(np.fliplr(ixs_i)))

    ixs_j = m + np.reshape(np.arange(0, n), (1,n))
    dia_j = ixs_j.diagonal()
    triu_j = np.fliplr(np.triu(np.fliplr(ixs_j)))
    diagonal = list(zip(*dia_i.tolist(), *dia_j.tolist()))
    triu = []
    tril = []
    for rowix, (irow, jrow) in enumerate(zip(triu_i.tolist(), triu_j.tolist())):
        triu.append([(0,0)] if rowix == 0 else [])
        tril.append([])
        for colix, (i, j) in enumerate(zip(irow, jrow)):
            if (i, j) in diagonal or ((i, j) == (0, 0) and rowix == 0):
                pass
            elif i > 0 or j > 0:
                triu[rowix].append((i, j))
            else:
                tril[rowix].append((rowix, colix))
    return (triu, diagonal, tril)

if __name__ == "__main__":
    def probfn3x3(i, j):
       match (i, j):
           case (0, 0): return [0.5]
           case (0, _): return [1./3, 1./4]
           case (_, 0): return [1./5, 1./6]
           case (1, 1): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
           case (2, 1): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
           case (1, 2): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
           case (2, 2): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case _: raise Exception

    #print(mkgrid(3, parentnames, probfn3x3, returnfn))
    #print(mkgrid(3, parentnames, probfn3x3, returnfn, sample=True))
    def probfn6x6(i, j):
       match (i, j):
           case (0, 0): return [0.5]
           case (_, 0): return [1./5, 1./6]
           case (0, _): return [1./3, 1./4]
           case (1, _): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
           case (2, _): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
           case (3, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
           case (4, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case (5, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case _: raise Exception()

    #print(mkgrid(6, parentnames, probfn6x6, returnfn))
    #print(mkgrid(6, parentnames, probfn6x6, returnfn, sample=True))

    def probfn9x9(i, j):
       match (i, j):
           case (0, 0): return [0.5]
           case (_, 0): return [1./5, 1./6]
           case (0, _): return [1./3, 1./4]
           case (1, _): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
           case (2, _): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
           case (3, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
           case (4, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case (5, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case (6, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case (7, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case (8, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
           case _: raise Exception()

    #print(mkgrid(9, parentnames, probfn9x9, returnfn))
    print(mkgrid(9, parentnames, probfn9x9, returnfn, sample=True))
