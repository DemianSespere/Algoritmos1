import unittest
from queue import Queue as Cola

from template_t1 import torneo_de_gallinas, reordenar_cola_priorizando_vips, cuantos_sufijos_son_palindromos, quien_gano_el_tateti_facilito

class Ej1Test(unittest.TestCase):
    def test_trivial(self):
        entrada = {"jugador1": "me desvio siempre"}
        salida = {"jugador1": 0}
        res = torneo_de_gallinas(entrada)
        self.assertEqual(res, salida)

class Ej2Test(unittest.TestCase):
    def test_trivial(self):
        entrada = Cola()
        entrada.put(("a","vip")) 
        salida = "a"
        res = reordenar_cola_priorizando_vips(entrada)
        nombre = res.get()[0]
        self.assertEqual(nombre, salida)

class Ej3Test(unittest.TestCase):
    def test_trivial(self):
        entrada = "Diego" 
        salida = 1
        res = cuantos_sufijos_son_palindromos(entrada)
        self.assertEqual(res,salida)


class Ej4Test(unittest.TestCase):
    def test_trivial(self):
        entrada = [
            ['X', 'O', ' ', ' ', ' '],
            ['X', 'O', ' ', ' ', ' '],
            ['X', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', 'O', 'O'],
            [' ', ' ', ' ', 'O', 'O']
        ]
        salida = 1
        res = quien_gano_el_tateti_facilito(entrada)
        self.assertEqual(res, salida)


if __name__ == '__main__':
    unittest.main(verbosity=2)
