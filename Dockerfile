FROM racket/racket
RUN raco pkg install --batch --auto compiler-lib https://github.com/jackfirth/resyntax.git https://github.com/9999years/racket-package-resyntax-action.git
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
