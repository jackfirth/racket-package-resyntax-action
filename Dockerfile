FROM racket/racket
RUN raco pkg install --batch --auto compiler-lib resyntax
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
