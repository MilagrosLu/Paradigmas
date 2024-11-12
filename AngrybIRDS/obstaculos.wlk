class Obstaculo {
    const property distanciaIslaPajaro
}
class Paredes {
    const ancho
    const material
    method resistencia () = ancho * material.factor()
}
object vidrio {
    const property factor = 10
}
object madera {
    const property factor = 25
}
object piedra {
    const property factor = 50
}
class Armadura {
    const property resistencia
}
class CerditoArmado {
    const armadura 
    method resistencia () = 10 * armadura.resistencia()
}

object ceriditoObrero {
    method resistencia () = 50
}