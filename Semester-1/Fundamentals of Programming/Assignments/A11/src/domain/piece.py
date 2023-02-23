class Piece:

    def __init__(self, owner, piece_color, display_text):
        self._owner = owner
        self._piece_color = piece_color    
        self._display_text = display_text


    @property
    def owner(self):
        """
        :return: The owner of the piece
        """
        return self._owner


    @property
    def color(self):
        """
        :return: The color of the piece
        """
        return self._piece_color


    @property
    def display_text(self):
        """
        :return: The display text of the piece
        """
        return self._display_text


    def __str__(self):
        """
        :return: A string representing this piece's owner
        """
        return str(self._owner)


    def __eq__(self, _object):
        """
        Checks if an object is the same as the current `Piece`
        :param _object: The object we want to check
        :return: True if they are equal, False otherwise
        """
        if not isinstance(_object, Piece):
            return False

        return self.owner == _object.owner
