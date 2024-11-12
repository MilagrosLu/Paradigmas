class Linea{
    const property packs
    const property consumos
    const tipoLinea
    var deuda = 0

    method consumosEnPeriodo (inicial,final) = consumos.filter{consumo => consumo.estaEnRango(inicial, final)}
    
    method promedioCostoConsumos (inicial,final) {
        const consumosDelRango = self.consumosEnPeriodo(inicial, final)
        return self.promedioCostos(consumosDelRango)

    }
    method promedioCostos (listaConsumos) {
        const sumaPrecios = listaConsumos.sum{consumo => consumo.costo()}
        return sumaPrecios / listaConsumos.size()
    }

    method gastoLineaUltimos30Dias(){
        const consumosEnUltimos30Dias = self.consumosEnPeriodo(hoy.minusDays(30), hoy)
        return consumosEnUltimos30Dias.sum{consumo => consumo.costo()}
    }
    method agregarPack (pack) {
        packs.add(pack)
    }
    method packsDisponibles (dia) = packs.filter {pack => pack.packDisp(dia)}
    method tienePacksParaConsumir (consumo) {
        const packsDisponibles = self.packsDisponibles(consumo.fecha())
        return packsDisponibles.any({pack => pack.puedeSatisfacer(consumo)})
    }

    method realizarConsumo (consumo) {
        if (self.tienePacksParaConsumir(consumo)) {
            const pack = self.primerPackQueSatisfase(consumo)
            pack.consumirse(consumo)
        }
        else {
            tipoLinea.noTienePackParaConsumo (self, consumo)
        }
        consumos.add(consumo)
    }
   
    method limpiezaDePacks() {
       packs.removeAllSuchThat({pack => !pack.disponible(hoy)})
    }
    method primerPackQueSatisfase (consumo) {
        return self.packsDisponibles(consumo.fecha()).findOrElse({pack => pack.puedeSatisfacer(consumo)},{} )
    }
    method aumentarDeuda(cantidad) {
        deuda += cantidad
    }

}

const hoy = new Date() //establece el dia actual

object comun {
    method noTienePackParaConsumo (linea ,consumo) {
        throw new DomainException (message = "No hay pack que pueda satisfacer el consumo")
    }
}
object black {
     method noTienePackParaConsumo (linea ,consumo) {
        linea.aumentarDeuda(consumo.costo())
    }
}
object platinum {
    method noTienePackParaConsumo (linea ,consumo) {
        // no hace nada deja que se consuma 
    }
}