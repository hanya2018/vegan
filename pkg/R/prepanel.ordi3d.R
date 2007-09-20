`prepanel.ordi3d` <-
    function(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1,1),  ...)
{
    require(lattice) || stop("requires 'lattice'")
    aspect = c(diff(ylim)/diff(xlim), diff(zlim)/diff(xlim))
    if (!exists("prepanel.default.cloud", mode="function")) {
        ## R 2.3.0 (in my MacOS 10.3.9) does not yet have prepanel.default.cloud.
        ## This lattice function is copied below to be used in older R.
        ## -- I hate setting package dependencies because of a single function. -- 
        message("vegan supplied version of prepanel default function used")
        prepanel.default.cloud <-
            function (perspective = TRUE, distance = if (perspective) 0.2 else 0, 
                      xlim, ylim, zlim, screen = list(z = 40, x = -60), R.mat = diag(4), 
                      aspect = c(1, 1), panel.aspect = 1, ..., zoom = 0.8) 
            {
                rot.mat <- lattice:::ltransform3dMatrix(screen = screen, R.mat = R.mat)
                aspect <- rep(aspect, length = 2)
                corners <- rbind(x = c(-1, 1, 1, -1, -1, 1, 1, -1),
                                 y = c(-1, -1, -1, -1, 1, 1, 1, 1) * aspect[1],
                                 z = c(-1, -1, 1, 1, -1, -1, 1, 1) * aspect[2])
                corners <- corners/(2 * max(corners))
                corners <- lattice:::ltransform3dto3d(corners, rot.mat, dist = distance)
                xrng <- range(corners[1, ])
                yrng <- range(corners[2, ])
                slicelen <- max(diff(xrng), diff(yrng)/panel.aspect)
                list(xlim = lattice:::extend.limits(xrng, length = slicelen)/zoom, 
                     ylim = lattice:::extend.limits(yrng, length = panel.aspect * slicelen)/zoom, 
                     dx = 1, dy = 1)
            }
    }
    prepanel.default.cloud(xlim = xlim, ylim = ylim, zlim = zlim, aspect = aspect, ...)
}
