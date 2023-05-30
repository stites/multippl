
@[extern "bern"] opaque bern : Float → Bool


def main : IO Unit :=
  IO.println $ bern 0.3
  -- map (_ => IO.println $ sampleBernoulli 0.3) [0,1,2,3,4,5,6,7,8,9] --

#eval main
