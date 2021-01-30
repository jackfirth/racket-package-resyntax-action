FROM racket/racket
RUN raco pkg install --batch --auto compiler-lib 	https://github.com/jackfirth/resyntax.git
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
