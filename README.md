# h42n42V2

Install opam with `bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"`

Run `opam init`

Run `opam install ocsipersist-sqlite-config eliom`

Make the project `Make test.opt`

Connect to `localhost:8080` and play.

----------------------------------

# The game
![Capture d’écran du 2024-09-29 15-49-09](https://github.com/user-attachments/assets/51675fae-a235-4220-84db-98c1bcc0c782)

This game consist of keeping away healthy creet (green ones) far from the river. If they get sick you can grab and drop your creet in the hospital.
![Capture d’écran du 2024-09-29 15-49-53](https://github.com/user-attachments/assets/d344cbb2-aa5c-45ca-9e23-99b1b3e95cbc)

If every creets get sick, you lose.
![Capture d’écran du 2024-09-29 15-49-53](https://github.com/user-attachments/assets/d7f177da-fc13-49b8-a0eb-464312e58cd7)

Survive a long as you can !

-----------------------------------

# For hosting

You can use `ocsigenserver -d`, the option `-d` is used to detach mode. 

For that you need to create a conf file, rewrite your xxx_main.eliom in an `ocsigen conf way`

Next to serv something you need to have a `index.html` file, so you can have it by running your makefile as test, curl on your website to get the html file copy it to your static folder

You can test mine here `https://h42n42.leo-giband.com`
