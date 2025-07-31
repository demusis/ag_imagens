# Ferramenta de Análise Geométrica sobre Imagens

Aplicativo **Shiny** que permite carregar uma fotografia, inserir construções projetivas (pontos, retas, pontos no infinito) e medir distâncias reais usando **razão cruzada**.  
Ideal para arquitetura, arqueologia digital, levantamento de fachadas ou qualquer tarefa em que você precise extrair medidas de cenas em perspectiva apenas com uma imagem.

---

## Recursos Principais
| Categoria | Descrição |
|-----------|-----------|
| **Upload de imagem** | Suporte a `png`, `jpeg` e `bmp` (até **30 MB**) |
| **Desenho** | - Pontos finitos<br>- Pontos no infinito (direções)<br>- Retas (finitas ou prolongadas)<br>- Pontos de interseção automáticos |
| **Medição** | - **Escala simples** (1 ponto no infinito + 2 pontos-referência)<br>- **Razão cruzada completa** (4 pontos finitos) |
| **Edição** | Paleta de cores, tamanho de símbolos, espessura de linhas, rótulos, prolongamento de retas |
| **Zoom & Canvas** | Zoom contínuo (25 % – 250 %), margens configuráveis |
| **Temas** | Interface moderna com **`shinythemes::flatly`** + CSS personalizado |
| **Log de operações** | Histórico de tudo o que foi feito na sessão |

---

## Pré-requisitos

| Requisito | Versão mínima |
|-----------|---------------|
| R | 4.0 |
| pacotes | `shiny`, `magick`, `colourpicker`, `shinythemes`<br><sup>(instalados automaticamente no 1º run, se desejar)</sup> |
| Sistema operacional | Windows, macOS ou Linux |

---

## Instalação

```r
# No R ou RStudio
install.packages(c("shiny", "magick", "colourpicker", "shinythemes"))
# magick pode exigir ImageMagick no sistema:
#   macOS: brew install imagemagick
#   Debian/Ubuntu: sudo apt-get install libmagick++-dev
