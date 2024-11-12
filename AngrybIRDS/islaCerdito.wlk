class IslaCerdito {
    const obstaculos 

    method recibeAtaque(pajaro) {
        const obstaculoMasCerca = self.obstaculoMasCercano()
        if( pajaro.puedeDerribar(obstaculoMasCerca)) {
            self.eliminarObstaculo(obstaculoMasCerca)
        }
    }
    method eliminarObstaculo (obstaculo) = obstaculos.remove(obstaculo)
    method obstaculoMasCercano () = obstaculos.min({obstaculo => obstaculo.distanciaIslaPajaro()})
    method sinObtaculos() = obstaculos.isEmpty()
}