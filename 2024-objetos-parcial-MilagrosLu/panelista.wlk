class Panelista {
    var puntosEstrella 
    method puntosEstrella() = puntosEstrella
    method aumentarPuntosEstrella (cantidad){
        puntosEstrella += cantidad
    }
    method darRemateGraciosoSobre(unaTematica) {
        self.aumentarPuntosEstrella(self.puntosSumadosPorRemate(unaTematica))
    }
    method puntosSumadosPorRemate(unaTematica)

    method opinar(unaTematica) {
        unaTematica.opinarsePor(self)
    }
    method realizarOpinion(unaTematica) {
        self.aumentarPuntosEstrella(self.puntosPorOpinar(unaTematica))
    }
    method puntosPorOpinar(unaTematica) = 1

    method emitirTematicaEnPrograma (unaTematica) {
        self.opinar(unaTematica)
        self.darRemateGraciosoSobre(unaTematica)
    }

    
}
class Celebridad inherits Panelista {
    override method puntosSumadosPorRemate(unaTematica) = 3
    override method puntosPorOpinar(unaTematica) = unaTematica.puntosPorOpinarFarandula(self)
    
}
class Colorado inherits Panelista  {
    var gracia
    override method puntosSumadosPorRemate(unaTematica) = gracia / 5
    override method darRemateGraciosoSobre(unaTematica) {
        super(unaTematica)
        self.aumentarGracia(1)
    }
    method aumentarGracia(cantidad) {
        gracia += cantidad
    }
}
class ColoradoConPeluca inherits Colorado {
    override method puntosSumadosPorRemate(unaTematica) = super(unaTematica) + 1
}
class Viejo inherits Panelista {
    override method puntosSumadosPorRemate(unaTematica) = unaTematica.cantidadPalabrasTitulo()
}
class PanelistaDeportivo inherits Panelista {
    override method puntosSumadosPorRemate(unaTematica) = 0
    override method puntosPorOpinar(unaTematica) = unaTematica.puntosExtraoridariosDeportivos()
}
