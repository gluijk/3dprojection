# 3D animation with music
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

translate = function(v, dx=0, dy=0, dz=0) {  # translation
    Tr=c(dx, dy, dz)
    v + Tr
}

scale = function(v, sx=1, sy=1, sz=1) {  # scale
    Sc=c(sx, sy, sz)
    v * Sc
}

rotateX = function(v, theta=0) {  # rotation around X axis
    Ro=matrix(c(1, 0,           0,
                0, cos(theta), -sin(theta),
                0, sin(theta),  cos(theta)),
              3, 3, byrow=TRUE)
    Ro %*% v  # matrix multiplication
}

rotateY = function(v, theta=0) {  # rotation around Y axis
    Ro=matrix(c(cos(theta), 0, sin(theta),
                0,          1, 0,
               -sin(theta), 0, cos(theta)),
              3, 3, byrow=TRUE)
    Ro %*% v  # matrix multiplication
}

rotateZ = function(v, theta=0) {  # rotation around Z axis
    Ro=matrix(c(cos(theta), -sin(theta), 0,
                sin(theta),  cos(theta), 0,
                0,           0,          1),
              3, 3, byrow=TRUE)
    Ro %*% v  # matrix multiplication
}

create.cube = function(x=0, y=0, z=0, Nx=3, Ny=3, Nz=3, Rx=1, Ry=1, Rz=1) {  # create list of cube points
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

draw.ball = function(img, x, y, z, R, f, zoom=50, val=1) {
    factor=f/z*zoom
    xp=x*factor+ncol(img)/2
    yp=y*factor+nrow(img)/2
    Rp=R*factor
    
    img=DrawCircle(img, xp, yp, Rp, inc=FALSE, fill=TRUE, val=0)
    img=DrawCircle(img, xp, yp, Rp, inc=FALSE, fill=FALSE, val=val)
    return(img)
}



# 'MYSTERY BALLS' ANIMATION

N=262  # 65  # 524  # 66  # 131  # number of frames
IMAGESIZE=800  # animation dimensions in pixels
SIZE=3  # number of balls per axis

centred.cube=create.cube(Nx=SIZE, Ny=SIZE, Nz=SIZE)  # (0,0,0) centred cube

# INITFRAME=0  # expanse (N=131)
# INITFRAME=131  # rotate Y (N=131)
# INITFRAME=262  # rotate X (N=131)
# INITFRAME=393  # rotate Z (N=131)
# INITFRAME=524  # pause 1 (N=66)
# INITFRAME=590  # rotate Y+X (N=524)
# INITFRAME=1114  # pause 2 (N=65)
INITFRAME=1179  # implosion (N=262)
for (t in 0:(N-1)) {
    print(paste0(t+1, "/", N))
    theta=0  #2*pi*t/N  # pi/2*t/N
    
    cube=centred.cube
    # cube=lapply(cube, rotateY, theta=theta)
    # cube=lapply(cube, rotateX, theta=theta)
    # cube=lapply(cube, rotateZ, theta=theta)
    cube=lapply(cube, translate, dz=10+((N-t)/N)^2*200)

    NBALLS=length(cube)
    dist=array(0,NBALLS)
    for (i in 1:NBALLS) {
        dist[i]=(cube[[i]][1]^2+cube[[i]][2]^2+cube[[i]][3]^2)^0.5
    }
    pos=order(dist, decreasing=TRUE)

    img=NewBitmap(IMAGESIZE, IMAGESIZE)
    for (i in 1:NBALLS) {
        img=draw.ball(img,
                      x=cube[[pos[i]]][1],
                      y=cube[[pos[i]]][2],
                      z=cube[[pos[i]]][3],
                      # val=(t+1)/N,
                      # R=0.8, f=5, zoom=100)
                      # R=0.8*t/N, f=5, zoom=100)
                      R=0.8*(N-1-t)/N, f=5, zoom=100)
    }
    
    frame=INITFRAME+t
    SaveBitmap(img, paste0("img",
                           ifelse(frame<10, "000",
                                  ifelse(frame<100, "00",
                                         ifelse(frame<1000, "0", ""))), frame)
               )
}

# Create final MP4 video:
# (Music: Dark Mystery Trailer (Taking Our Time) by AlexGrohl)
# ffmpeg -framerate 30 -i img%4d.png -i darkmystery.wav -c:v libx264 -crf 15
# -pix_fmt yuv420p 3danimation.mp4

