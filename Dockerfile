FROM racket/racket:8.0-full
RUN raco pkg install --batch --auto --scope installation \
         "https://github.com/9999years/rebellion.git#remove-reprovide-dep" \
         "https://github.com/jackfirth/resyntax.git" \
         "https://github.com/9999years/racket-package-resyntax-action.git#main"
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
