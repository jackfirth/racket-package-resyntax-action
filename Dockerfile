FROM racket/racket
RUN raco pkg install --batch --auto \
         compiler-lib \
         https://github.com/jackfirth/resyntax.git
RUN raco pkg install --batch --auto \
         "https://github.com/9999years/racket-package-resyntax-action.git#main"
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
