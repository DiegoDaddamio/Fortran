#Pour utiliser ce code il suffit de glisser le fichier txt SUR ce fichier .py
import matplotlib.pyplot as plt #installer la lib en mettant "pip install matplotlib" dans le cmd
import sys

def plot_from_file(file_name):
    try:
        # Ouvrir le fichier en lecture
        with open(file_name, 'r') as file:
            data = file.readlines()

        x = []
        y = []

        # Parcourir les lignes du fichier et extraire les données
        for line in data:
            line = line.strip().split()
            if len(line) == 2:
                x.append(float(line[0]))
                y.append(float(line[1]))

        # Tracer une ligne reliant les points
        plt.plot(x, y, marker='o', linestyle='-', label='Points', color='b')

        # Personnaliser le graphique
        plt.title('Graphique des points')
        plt.xlabel('X')
        plt.ylabel('Y')
        plt.grid(True)

        # Afficher le graphique
        plt.legend()
        plt.show()

    except FileNotFoundError:
        print(f"Le fichier {file_name} n'a pas été trouvé.")
    except Exception as e:
        print(f"Une erreur s'est produite : {e}")

if __name__ == "__main__":
    if len(sys.argv) == 2:
        file_name = sys.argv[1]
        plot_from_file(file_name)
    else:
        print("Utilisation : python script.py <fichier.txt>")