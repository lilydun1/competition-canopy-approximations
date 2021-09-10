

I0 -- light above canopy

# one stream model

L = leaf area above leaf (i..e amoutn of shading)
I - light on leaf

I = I0 * exp(-kL)

so if L = 5 (floor of rainforest)

I/I0 = exp(-0.5*5) = 0.082

# two stream

there's fraction of leaf that is shaded, fraction unshaded

f_un = exp(-kL) ---> i..e 8% of leaf is in direct light

f_sh = 1-f_un = 1- exp(-kL)

light on unshaed: I_un = I0
light on shaded: I_sh  = I0 exp(-kL)

av L = (f_un I_un + f_sh * I_sh ) / (f_un + f_sh) = f_un I_un + f_sh * I_sh
  = exp(-kL) I0 + (1- exp(-kL)) * I0exp(-kL)
  = I0exp(-kL) (1 + 1 - exp(-kL))
  = I0exp(-kL) (2 - exp(-kL))

I/I0 = exp(-kL) (2 - exp(-kL)) -> 0.082*(2-0.082) = 0.15

