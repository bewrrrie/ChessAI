if [ ! -e ChessAI ]; then
    echo "Compiling executable binary..."
    stack build
    find .stack-work/install -name ChessAI-exe -exec cp {} ./ChessAI \;
fi

./ChessAI
