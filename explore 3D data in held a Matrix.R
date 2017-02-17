
data("volcano")

class(volcano)
# [1] Matrix


# plot images of 3D data

# as a 2D contour lines map
contour(volcano)

# as a 3D perspective sketch on x-y-z axes
persp(volcano)             # chart is too spiky...
persp(volcano,expand=0.2)  # use expand arg to flatten the chart (scale the Z axis)

# as a 2D heat map
image(volcano) 
