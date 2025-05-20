# LDD IPF Huff

## Projet Compression de texte

### Membres
- Reina Al Masri  
- Maram Sall Gueye  

---

### Description
Ce programme permet de compresser et de décompresser des fichiers texte à l’aide de l’algorithme de compression de Huffman.  
Pour plus de détails, veuillez consulter le [rapport du projet]((https://github.com/reina-m/Huffman/blob/main/Rapport_du_Projet_Huffman.pdf)).

---

### Utilisation

#### Compilation
Pour compiler le programme, deux options sont disponibles :

- **Avec Dune** :  
  ```bash
  dune build
  ```
  Le fichier exécutable sera généré sous le nom `./huff.exe`.

- **Avec `ocamlc`** :  
  ```bash
  ocamlc -o huff heap.ml bs.ml huffman.ml huff.ml
  ```
  Le fichier exécutable sera généré sous le nom `./huff`.

#### Commandes principales
1. **Afficher l’aide** :  
   ```bash
   ./huff --help
   ```
   Affiche un message listant les options disponibles.  

2. **Compresser un fichier** :  
   ```bash
   ./huff <fichier>
   ```
   Crée une version compressée `<fichier>.hf` du fichier.  

3. **Afficher les statistiques de compression** :  
   ```bash
   ./huff --stats <fichier>
   ```
   Crée une version compressée `<fichier>.hf` tout en affichant des statistiques, comme la taille initiale, la taille compressée et le taux de compression.  

4. **Décompresser un fichier compressé** :  
   ```bash
   ./huff <fichier>.hf
   ```
   Crée une version décompressée du fichier compressé, en supprimant l’extension `.hf`.  

---

### Exemple de résultat

Voici un exemple d’exécution avec un fichier texte (`Le_parfum_Suskind.txt`) :  

```bash
./huff --stats Le_parfum_Suskind.txt
```

**Résultat** :  
```
Compression en cours pour le fichier : Le_parfum_Suskind.txt  
Compression terminée.  
Statistiques de compression :  
- Taille originale : 553872 octets  
- Taille compressée : 316152 octets  
- Taux de compression : 42.92%  
```

---

### Fichiers inclus dans le projet

- **bs.mli / bs.ml** : Gestion des flux binaires.  
- **heap.mli / heap.ml** : Gestion et manipulation des tas (heap), nécessaires pour la construction de l’arbre de Huffman.  
- **huffman.ml** : Fonctions principales pour la compression et la décompression.  
- **huff.ml** : Interface ligne de commande.  
- **test.ml** : Tests réalisés pour vérifier les fonctionnalités.  
- **Le_parfum_Suskind.txt** : Fichier texte utilisé pour tester la compression et la décompression.  

---

### Remarques et améliorations possibles
Bien que le projet offre une compression efficace (> 40 % dans certains cas), une optimisation plus poussée pourrait être envisagée pour réduire davantage la taille des fichiers compressés.
