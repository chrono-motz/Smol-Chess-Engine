import ctypes
engine = ctypes.CDLL("/kaggle_simulations/agent/engine_2.so")
engine.engine.argtypes = [ctypes.c_char_p, ctypes.c_int]
engine.engine.restype = ctypes.c_char_p
engine.initialize.argtypes = [ctypes.c_int]
engine.initialize(1)
def chess_bot(obs):
    fen_string = obs.board.encode('utf-8')
    uci_move = engine.engine(fen_string,2)
    move_str = ctypes.c_char_p(uci_move).value.decode('utf-8')
    return move_str