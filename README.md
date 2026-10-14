# compgeomlab

Petit laboratoire de **géométrie algorithmique** en OCaml, avec une fenêtre
graphique [raylib](https://github.com/tjammer/raylib-ocaml) (Marc de Falco,
2022-2023).

On clique dans la fenêtre pour ajouter des points ; l'enveloppe convexe du
nuage est recalculée et dessinée à chaque clic. Le clic droit efface tout.

## Contenu

| Fichier | Rôle |
|---|---|
| `basic.ml` | Outils géométriques : points et vecteurs (`Point`), demi-droites (`Ray`), boîtes alignées (`AABB`), triangles (orientation, centre du cercle circonscrit), polygones convexes, intersections |
| `convexHull.ml` | Enveloppe convexe par l'algorithme de **Jarvis** (paquet cadeau), en O(nh) |
| `voronoi.ml` | Diagramme de **Voronoï** : **inachevé**. Types des cellules (bornées ou non), tracé d'une cellule dans une boîte, tangente commune à deux enveloppes ; l'insertion incrémentale et la fusion « diviser pour régner » ne sont pas écrites (la fusion est en commentaire) |
| `compgeomlab.ml` | Programme principal : fenêtre 1000 × 1000, saisie des points à la souris, dessin |

## Compilation

Il faut OCaml (testé avec 5.4), dune et la bibliothèque `raylib` :

```console
$ opam install dune raylib
$ dune build
$ ./_build/default/compgeomlab.exe
```

## État

* L'enveloppe convexe est correcte, y compris avec des points alignés ou
  confondus. Elle est rendue sous forme de liste dont le dernier élément
  répète le premier.
* `Voronoi.compute` lève une exception : son appel est commenté dans
  `compgeomlab.ml`.
