# Linear projection of a 3D scene onto a plane
# www.overfitting.net
# https://www.overfitting.net/2023/01/proyeccion-lineal-de-escenas-3d-sobre.html


# LIBRERÍA GRÁFICA BITMAP

NewBitmap = function(dimx, dimy, val=0) {
    # Crea bitmap de dimensiones dimx y dimy
    return(array(val,c(dimx,dimy)))
}

DrawEllip = function(img, x0, y0, a, b, inc=TRUE, val=1, fill=FALSE, thick=1) {
    # Dibuja elipse de centro (x0,y0) y radios a y b
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    # Aquí no redondeamos para tener más precisión en la división
    if (fill) {
        indices=which( ((row(img)-x0)/a)^2 + ((col(img)-y0)/b)^2 < 1 )
    } else {
        indices=which( ((row(img)-x0)/(a+thick/2))^2 + ((col(img)-y0)/(b+thick/2))^2 <  1 &
                       ((row(img)-x0)/(a-thick/2))^2 + ((col(img)-y0)/(b-thick/2))^2 >= 1 )
    }
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawCircle = function(img, x0, y0, r, inc=TRUE, val=1, fill=FALSE, thick=1) {
    # Dibuja círculo de centro (x0,y0) y radio r
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    img=DrawEllip(img, x0, y0, r, r, inc, val, fill, thick)
    
    return(img)
}

SaveBitmap = function(img, name, trunc=TRUE, gamma=1) {
    # Guarda bitmap en formato PNG
    # Solo si trunc=FALSE y la imagen excede de 1 se reescala a 1
    library(png)
    img[img<0]=0
    if (trunc) img[img>1]=1
    if (tolower(substr(name, nchar(name)-3, nchar(name))) != ".png") name=paste0(name,".png")
    writePNG(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), name)
}



# BASIC 3D FUNCTIONS

translate=function(v, dx=0, dy=0, dz=0) {  # translation
    Tr=c(dx, dy, dz)
    v + Tr
}

scale=function(v, sx=1, sy=1, sz=1) {  # scale
    Sc=c(sx, sy, sz)
    v * Sc
}

rotateX=function(v, theta=0) {  # rotation around X axis
    Ro=matrix(c(1, 0,           0,
                0, cos(theta), -sin(theta),
                0, sin(theta),  cos(theta)),
              3, 3, byrow=TRUE)
    Ro %*% v  # matrix multiplication
}

rotateY=function(v, theta=0) {  # rotation around Y axis
    Ro=matrix(c(cos(theta), 0, sin(theta),
                0,          1, 0,
               -sin(theta), 0, cos(theta)),
              3, 3, byrow=TRUE)
    Ro %*% v  # matrix multiplication
}

rotateZ=function(v, theta=0) {  # rotation around Z axis
    Ro=matrix(c(cos(theta), -sin(theta), 0,
                sin(theta),  cos(theta), 0,
                0,           0,          1),
              3, 3, byrow=TRUE)
    Ro %*% v  # matrix multiplication
}

create.cube=function(x=0, y=0, z=0, Nx=3, Ny=3, Nz=3, Rx=1, Ry=1, Rz=1) {  # create list of cube points
    cube=list()
    n=1
    for (i in 1:Nx) {
        for (j in 1:Ny) {
            for (k in 1:Nz) {
                cube[[n]]=matrix(c(x+(i-1)*2*Rx-(Nx-1)*Rx,
                                   y+(j-1)*2*Ry-(Ny-1)*Ry,
                                   z+(k-1)*2*Rz-(Nz-1)*Rz),
                                 3, 1)
                n=n+1
            }
        }
    }
    return(cube)
}

draw.ball=function(img, x, y, z, R, f, zoom=50, val) {
    factor=f/z
    xp=x*factor*zoom+ncol(img)/2
    yp=y*factor*zoom+nrow(img)/2
    Rp=R*factor*zoom
    
    img=DrawCircle(img, xp, yp, Rp, inc=FALSE, val=val, fill=TRUE)
    # img=DrawCircle(img, xp, yp, Rp, val=0.8, fill=FALSE)
    return(img)
}



# EXAMPLES

# single point
v=matrix(1:1, 3, 1)  # single point
scale(v, sy=10)
translate(v, dy=10)
rotateZ(v, theta=pi/2)

# list of points -> lapply()
v=list(v, -v*2)  # list of points
lapply(v, scale, sy=10)
lapply(v, translate, dy=10)
lapply(v, rotateZ, theta=pi/2)



# 3D BALLS ANIMATION

N=360  # number of frames
MIN=5.6178161304326  # closest ball obtained trough a pre-simulation
MAX=14.7797206308053  # farthest ball obtained trough a pre-simulation
for (t in 0:(N-1)) {
    SIZE=3  # number of balls per axis
    cube=create.cube(Nx=SIZE+2, Ny=SIZE+1, Nz=SIZE)  # (0,0,0) centred cube
    
    theta=2*pi*t/N
    cube=lapply(cube, rotateY, theta=theta)
    cube=lapply(cube, rotateZ, theta=theta)
    cube=lapply(cube, translate, dz=10)
    img=NewBitmap(800, 800, val=1)
    
    NBALLS=length(cube)
    dist=array(0,NBALLS)
    for (i in 1:NBALLS) {
        dist[i]=(cube[[i]][1]^2+cube[[i]][2]^2+cube[[i]][3]^2)^0.5
    }
    pos=order(dist, decreasing=TRUE)

    # MIN=min(dist)
    # MAX=max(dist)
    for (i in 1:NBALLS) {
        img=draw.ball(img,
                      x=cube[[pos[i]]][1],
                      y=cube[[pos[i]]][2],
                      z=cube[[pos[i]]][3],
                      R=0.8, f=5, zoom=100,
                      val=(0.99-0)/(MAX-MIN)*(dist[pos[i]]-MIN))
    }
    
    SaveBitmap(img, paste0("img", ifelse(t<10, "00", ifelse(t<100, "0", "")), t))
}
