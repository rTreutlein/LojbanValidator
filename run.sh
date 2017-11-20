if [ "$1" == "t" ] ; then
    stack exec lojban-test
else
echo "Starting Chat Bot"
    stack exec lojbanChatBot
fi
