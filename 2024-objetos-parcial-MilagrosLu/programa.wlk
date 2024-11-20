class Emision {
    const tematicasATratar
    const panelistas 
    var emisionConcluida = false
    method sePuedeEmitir () = self.cantidadPanelistas() >= 2 && self.mayoriaTematicasInteresantes()
    method cantidadPanelistas() = panelistas.size()
    method cantidadTematicasInteresantes() = tematicasATratar.filter{tematica => tematica.esInteresante()}.size()
    method mayoriaTematicasInteresantes() =  self.cantidadTematicasInteresantes() / tematicasATratar.size() > 0.5

    method emitir() {
        tematicasATratar.forEach({tematica => self.emitirTematica(tematica)})
        emisionConcluida = true
    }
    method emitirTematica(unaTematica) {
        panelistas.forEach({panelista => panelista.emitirTematicaEnPrograma(unaTematica)})
    }
    method verificarSiTerminoEmision() {
        if (!emisionConcluida) {
            throw new DomainException (message = "La emision no ha concluido. No estan cerrados los resultados")
        }
    }
    method panelistaEstrella () {
        self.verificarSiTerminoEmision()
        return panelistas.max{panelista => panelista.puntosEstrella()}
    }
}