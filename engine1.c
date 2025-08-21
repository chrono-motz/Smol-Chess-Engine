#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>
#include <limits.h>
#include <stdbool.h>

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

typedef uint64_t U64;
typedef uint32_t U32;
typedef uint8_t U8;

typedef struct {
   U64 moves[8];
   U64 attacks[8];
   int square[8];
} PieceMoves;

const char piece_chars[6] = {'p', 'n', 'k', 'r', 'q', 'k'};

void printu64(U64 n){
    for(int i=7;i>=0;i--){
        for(int j=0;j<8;j++){
            printf("%d", (n >> (i*8+j)) & 1);
        }
        printf("\n");
    }
    printf("\n");
}

void square_to_chess(int square, char output[3]) {
    output[0] = 'a' + (square % 8);      // File (column)
    output[1] = '1' + (square / 8);      // Rank (row)
    output[2] = '\0';                    // Null terminator
}

void parse_fen(const char *fen, U64 board[2][6],U64 occupancy[2],char *active_color, char *castling_rights, 
               char *en_passant, int *halfmove_clock, int *fullmove_number) {
    // Initialize all bitboards to 0
    memset(board, 0, 2 * 6 * sizeof(U64));
    memset(occupancy, 0, 2 * sizeof(U64));
    memset(castling_rights, 0, 5);
    memset(en_passant, 0, 3);

    // Map piece characters to board indices
    const char piece_map[12] = {'P', 'N', 'B', 'R', 'Q', 'K', 'p', 'n', 'b', 'r', 'q', 'k'};
    const int color_map[12] = {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}; // 0 = white, 1 = black
    const int type_map[12] = {0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5};   // 0 = pawns, 1 = knights, etc.

    // Copy the FEN string for tokenization
    char fen_copy[128];
    strncpy(fen_copy, fen, sizeof(fen_copy) - 1);
    fen_copy[sizeof(fen_copy) - 1] = '\0';

    // Split the FEN string into parts
    char *token = strtok(fen_copy, " ");

    // 1. Piece Placement
    if (token) {
        int row = 7, col = 0; // FEN starts from the 8th rank (row 7 in 0-based index)
        for (int i = 0; token[i] != '\0'; ++i) {
            char c = token[i];
            if (c == '/') {
                row--;
                col = 0;
            } else if (c >= '1' && c <= '8') {
                col += c - '0'; // Skip empty squares
            } else {
                // Find the index of the piece in the board array
                for (int j = 0; j < 12; ++j) {
                    if (piece_map[j] == c) {
                        int color = color_map[j];
                        int type = type_map[j];
                        // Set the corresponding bit in the bitboard
                        board[color][type] |= (U64)1 << (row * 8 + col);
                        break;
                    }
                }
                col++;
            }
        }
        token = strtok(NULL, " ");
    }

    // 2. Active Color
    if (token) {
        *active_color = token[0];
        token = strtok(NULL, " ");
    }

    // 3. Castling Rights
    if (token) {
        strncpy(castling_rights, token, 4);
        token = strtok(NULL, " ");
    }

    // 4. En Passant Target Square
    if (token) {
        strncpy(en_passant, token, 2);
        token = strtok(NULL, " ");
    }

    // 5. Halfmove Clock
    if (token) {
        *halfmove_clock = atoi(token);
        token = strtok(NULL, " ");
    }

    // 6. Fullmove Number
    if (token) {
        *fullmove_number = atoi(token);
    }
    for(int i=0;i<2;i++){
        for(int j=0;j<6;j++){
            occupancy[i] |= board[i][j];
        }
    }
}


U64 king_attacks(int square){
    U64 attacks = (square < 9) ? 460039ULL >> (9 - square) : 460039ULL << (square - 9);
    return attacks & ((!(square  & 7)) ? 9187201950435737471ULL : ((square & 7) == 6) ? 18374403900871474942ULL : ~0ULL);
}
U64 knight_attacks(int square){
    U64 attacks = (square < 18) ? 43234889994ULL >> (18 - square) : 43234889994ULL << (square - 18);
    return attacks & (((square  & 6)==0) ? 4557430888798830399ULL : ((square & 6) == 6) ? 18229723555195321596ULL : ~0ULL);
}
U64 pawn_moves(int square, U64 own_pieces,U64 opponent_pieces, int colour) {
    U64 moves = 0;
    U64 combined_pieces = own_pieces | opponent_pieces ; 
    int forward = colour ? 8 : -8; 
    int double_forward = colour ? 16 : -16;
    int left_capture = colour ? (square % 8 != 0 ? 9 : 0) : (square % 8 != 0 ? -9 : 0);
    int right_capture = colour ? (square % 8 != 7 ? 7 : 0) : (square % 8 != 7 ? -7 : 0);

    if (!(combined_pieces & (1ULL << (square + forward)))) {
        moves |= 1ULL << (square + forward);
        if ((colour && square / 8 == 1) || (!colour && square / 8 == 6)) {
            if (!(combined_pieces & (1ULL << (square + double_forward)))) {
                moves |= 1ULL << (square + double_forward);
            }
        }
    }

    if (left_capture) {
        if (opponent_pieces & (1ULL << (square + left_capture))) {
            moves |= 1ULL << (square + left_capture);
        }
    }

    if (right_capture) {
        if (opponent_pieces & (1ULL << (square + right_capture))) {
            moves |= 1ULL << (square + right_capture);
        }
    }

    return moves & ~own_pieces;
}
U64 pawn_attacks(int square,int colour) {
    U64 attacks = 0;
    int left_capture = colour ? (square % 8 != 0 ? 9 : 0) : (square % 8 != 0 ? -9 : 0);
    int right_capture = colour ? (square % 8 != 7 ? 7 : 0) : (square % 8 != 7 ? -7 : 0);

    if (left_capture) {
        (attacks) |= 1ULL << (square + left_capture);
    }
    if (right_capture) {
        (attacks) |= 1ULL << (square + right_capture);
    }
    return attacks;
}
U64 rook_moves(int square, U64 own_pieces,U64 opponent_pieces) {
    U64 position = 1ULL << square;
    U64 occupancies_for_moves = own_pieces | opponent_pieces;
    occupancies_for_moves &= ~(1ULL << square); // Exclude the current square from occupancies_for_moves

    // Rank mask and file mask for the square
    U64 rank_mask = 0xFFULL << (square & ~7); // Mask for the rank
    U64 file_mask = 0x0101010101010101ULL << (square & 7); // Mask for the file

    // Rank calculations
    U64 rank_occ = occupancies_for_moves & rank_mask;         // Occupied squares in the rank
    U64 left_block = rank_occ & (position - 1);     // Pieces to the left
    U64 right_block = rank_occ & ~(position - 1);   // Pieces to the right
    U64 left_most = left_block ? 1ULL << (63 - __builtin_clzll(left_block)) : 1ULL;
    U64 right_most = right_block ? 1ULL << __builtin_ctzll(right_block) : 1ULL;
    U64 rank_moves = (((right_most<<1) - position) | (position - left_most)) & rank_mask;
    // File calculations
    U64 file_occ = occupancies_for_moves & file_mask;         // Occupied squares in the file
    U64 lower_block = file_occ & (position - 1);    // Pieces below
    U64 upper_block = file_occ & ~(position - 1);   // Pieces above
    U64 lower_most = lower_block ? 1ULL << (63 - __builtin_clzll(lower_block)) : 1ULL;
    U64 upper_most = upper_block ? 1ULL << __builtin_ctzll(upper_block) : 1ULL;
    U64 file_moves = (((upper_most<<1) - position) | (position - lower_most)) & file_mask;

    return (rank_moves | file_moves) & ~own_pieces;
}
U64 rook_attacks(int square,U64 own_pieces,U64 opponent_pieces,U64 rook_queen) {
    U64 position = 1ULL << square;
    U64 occupancies_for_attacks = (own_pieces | opponent_pieces) & ~(rook_queen);

    // Rank mask and file mask for the square
    U64 rank_mask = 0xFFULL << (square & ~7); // Mask for the rank
    U64 file_mask = 0x0101010101010101ULL << (square & 7); // Mask for the file

    // Rank calculations
    U64 rank_occ = occupancies_for_attacks & rank_mask;         // Occupied squares in the rank
    U64 left_block = rank_occ & (position - 1);     // Pieces to the left
    U64 right_block = rank_occ & ~(position - 1);   // Pieces to the right
    U64 left_most = left_block ? 1ULL << (63 - __builtin_clzll(left_block)) : 1ULL;
    U64 right_most = right_block ? 1ULL << __builtin_ctzll(right_block) : 1ULL;
    U64 rank_attacks = (((right_most<<1) - position) | (position - left_most)) & rank_mask;
    // File calculations
    U64 file_occ = occupancies_for_attacks & file_mask;         // Occupied squares in the file
    U64 lower_block = file_occ & (position - 1);    // Pieces below
    U64 upper_block = file_occ & ~(position - 1);   // Pieces above
    U64 lower_most = lower_block ? 1ULL << (63 - __builtin_clzll(lower_block)) : 1ULL;
    U64 upper_most = upper_block ? 1ULL << __builtin_ctzll(upper_block) : 1ULL;
    U64 file_attacks = (((upper_most<<1) - position) | (position - lower_most)) & file_mask;

    return (rank_attacks | file_attacks) & ~(1ULL << square);
}
U64 bishop_moves(int square, U64 own_pieces,U64 opponent_pieces) {
    U64 position = 1ULL << square;
    U64 occupancies_for_moves = own_pieces | opponent_pieces;
    
    int rank = square / 8;
    int file = square % 8;
    
    U64 diagonal_mask = (rank > file) ? 
        0x8040201008040201ULL << ((rank-file) * 8) :
        0x8040201008040201ULL >> ((file-rank) * 8);
    
    U64 anti_diagonal_mask = (rank+file <= 7) ?
        0x0102040810204080ULL >> ((7 - rank - file) * 8) :
        0x0102040810204080ULL << ((rank + file - 7) * 8);
    
    // Rest of your code remains the same
    U64 diag_occ = occupancies_for_moves & diagonal_mask;
    U64 lower_diag_block = diag_occ & (position - 1);
    U64 upper_diag_block = diag_occ & ~((position << 1) - 1);
    U64 lower_diag_most = lower_diag_block ? 1ULL << (63 - __builtin_clzll(lower_diag_block)) : 1;
    U64 upper_diag_most = upper_diag_block ? 1ULL << __builtin_ctzll(upper_diag_block) : 1;
    U64 diagonal_moves = (((upper_diag_most << 1) - position) | (position - lower_diag_most)) & diagonal_mask;
    
    U64 anti_diag_occ = occupancies_for_moves & anti_diagonal_mask;
    U64 lower_anti_diag_block = anti_diag_occ & (position - 1);
    U64 upper_anti_diag_block = anti_diag_occ & ~((position << 1) - 1);
    U64 lower_anti_diag_most = lower_anti_diag_block ? 1ULL << (63 - __builtin_clzll(lower_anti_diag_block)) : 1;
    U64 upper_anti_diag_most = upper_anti_diag_block ? 1ULL << __builtin_ctzll(upper_anti_diag_block) : 1;
    U64 anti_diagonal_moves = (((upper_anti_diag_most << 1) - position) | (position - lower_anti_diag_most)) & anti_diagonal_mask;
    
    return (diagonal_moves | anti_diagonal_moves) & ~own_pieces;
}
U64 bishop_attacks(int square,U64 own_pieces,U64 opponent_pieces,U64 bishop_queen) {
    U64 position = 1ULL << square;
    U64 occupancies_for_attacks = (own_pieces | opponent_pieces) & ~(bishop_queen);
    
    int rank = square / 8;
    int file = square % 8;
    
    U64 diagonal_mask = (rank > file) ? 
        0x8040201008040201ULL << ((rank-file) * 8) :
        0x8040201008040201ULL >> ((file-rank) * 8);
    
    U64 anti_diagonal_mask = (rank+file <= 7) ?
        0x0102040810204080ULL >> ((7 - rank - file) * 8) :
        0x0102040810204080ULL << ((rank + file - 7) * 8);
    
    U64 diag_occ = occupancies_for_attacks & diagonal_mask;
    U64 lower_diag_block = diag_occ & (position - 1);
    U64 upper_diag_block = diag_occ & ~((position << 1) - 1);
    U64 lower_diag_most = lower_diag_block ? 1ULL << (63 - __builtin_clzll(lower_diag_block)) : 1;
    U64 upper_diag_most = upper_diag_block ? 1ULL << __builtin_ctzll(upper_diag_block) : 1;
    U64 diagonal_attacks = (((upper_diag_most << 1) - position) | (position - lower_diag_most)) & diagonal_mask;
    
    U64 anti_diag_occ = occupancies_for_attacks & anti_diagonal_mask;
    U64 lower_anti_diag_block = anti_diag_occ & (position - 1);
    U64 upper_anti_diag_block = anti_diag_occ & ~((position << 1) - 1);
    U64 lower_anti_diag_most = lower_anti_diag_block ? 1ULL << (63 - __builtin_clzll(lower_anti_diag_block)) : 1;
    U64 upper_anti_diag_most = upper_anti_diag_block ? 1ULL << __builtin_ctzll(upper_anti_diag_block) : 1;
    U64 anti_diagonal_attacks = (((upper_anti_diag_most << 1) - position) | (position - lower_anti_diag_most)) & anti_diagonal_mask;
    
    return (diagonal_attacks | anti_diagonal_attacks) & ~(1ULL << square);
}


PieceMoves initialize_moves(U64 board[2][6], U64 own_pieces, U64 opponent_pieces,int piece_type,int colour){
    PieceMoves piece = {0};
    int i = 0;
    int square;
    U64 piece_board=board[colour][piece_type];
    while(piece_board) {
        square = __builtin_ctzll(piece_board);
        piece.square[i] = square;
        switch (piece_type)
        {
        case 0:
            piece.moves[i] = pawn_moves(square,own_pieces,opponent_pieces,colour);
            piece.attacks[i] = pawn_attacks(square,colour);
            break;
        case 1:
            piece.attacks[i] = knight_attacks(square);
            piece.moves[i] = piece.attacks[i] & ~own_pieces;
            break;
        case 2:
            piece.moves[i] = bishop_moves(square,own_pieces,opponent_pieces);
            piece.attacks[i] = bishop_attacks(square,own_pieces,opponent_pieces, board[colour][2] | board[colour][4] );
            break;
        case 3:
            piece.moves[i] = rook_moves(square,own_pieces,opponent_pieces);
            piece.attacks[i] = rook_attacks(square,own_pieces,opponent_pieces, board[colour][3] | board[colour][4] );
            break;
        case 4:
            piece.moves[i] = bishop_moves(square,own_pieces,opponent_pieces) | rook_moves(square,own_pieces,opponent_pieces);
            piece.attacks[i] = bishop_attacks(square,own_pieces,opponent_pieces,board[colour][2] | board[colour][4] ) | rook_attacks(square,own_pieces,opponent_pieces, board[colour][3] | board[colour][4] );
            break;
        case 5:
            piece.attacks[i] = king_attacks(square);
            piece.moves[i] = piece.attacks[i] & ~own_pieces;
            break;
        default:
            break;
        }

        i = i+1;
        piece_board &=  ~(1ULL<<square) ;
    }
    return piece;
}


// Evaluation constants
#define PAWN_VALUE 100
#define KNIGHT_VALUE 320
#define BISHOP_VALUE 330
#define ROOK_VALUE 500
#define QUEEN_VALUE 900

// Transposition table constants
#define TT_SIZE 285714  // 1M entries (~4MB for a 4-byte entry)
#define HASH_EXACT 0
#define HASH_ALPHA 1
#define HASH_BETA 2

// Search constants
#define MAX_PLY 64
#define MAX_KILLER_MOVES 2
#define INFINITY 30000

typedef struct {
    U64 key;
    int16_t score;
    uint8_t depth;
    uint8_t flag;
    char best_move[6];
} TTEntry;

// Global static memory
static TTEntry tt[TT_SIZE];  // Fixed transposition table

// Add move stack to SearchInfo structure
int killer_moves[MAX_PLY][MAX_KILLER_MOVES];
// int history[2][64][64];  // [color][from][to]
int pv_length[MAX_PLY];
char pv_table[MAX_PLY][MAX_PLY][6];

void reset_pv(){
    for(int i=0;i<MAX_PLY;i++){
        pv_length[i]=0;
        for(int j=0;j<MAX_PLY;j++){
            pv_table[i][j][0] = '\0'; 
        }
    }
}

void clear_tt() {
    memset(tt, 0, sizeof(TTEntry));
}

// Zobrist hashing
U64 piece_keys[2][6][64];
U64 side_key;
U64 ep_keys[64];

void init_hash_keys() {
    srand(time(NULL));
    for(int color = 0; color < 2; color++)
        for(int piece = 0; piece < 6; piece++)
            for(int sq = 0; sq < 64; sq++)
                piece_keys[color][piece][sq] = ((U64)rand() << 32) | rand();
    
    side_key = ((U64)rand() << 32) | rand();
    for(int sq = 0; sq < 64; sq++)
        ep_keys[sq] = ((U64)rand() << 32) | rand();
}

U64 generate_hash(U64 board[2][6], int side_to_move, char *en_passant) {
    U64 hash = 0;
    
    // Hash pieces
    for(int color = 0; color < 2; color++)
        for(int piece = 0; piece < 6; piece++)
            for(int sq = 0; sq < 64; sq++)
                if(board[color][piece] & (1ULL << sq))
                    hash ^= piece_keys[color][piece][sq];
    
    // Hash side to move
    if(side_to_move)
        hash ^= side_key;
    
    // Hash en-passant square
    if(en_passant[0] != '-') {
        int file = en_passant[0] - 'a';
        int rank = en_passant[1] - '1';
        hash ^= ep_keys[rank * 8 + file];
    }
    
    return hash;
}

// Simple evaluation function
int evaluate(U64 board[2][6], int side) {
    int score = 0;
    
    // Material counting
    score += PAWN_VALUE * __builtin_popcountll(board[1][0]);
    score += KNIGHT_VALUE * __builtin_popcountll(board[1][1]);
    score += BISHOP_VALUE * __builtin_popcountll(board[1][2]);
    score += ROOK_VALUE * __builtin_popcountll(board[1][3]);
    score += QUEEN_VALUE * __builtin_popcountll(board[1][4]);
    score -= PAWN_VALUE * __builtin_popcountll(board[0][0]);
    score -= KNIGHT_VALUE * __builtin_popcountll(board[0][1]);
    score -= BISHOP_VALUE * __builtin_popcountll(board[0][2]);
    score -= ROOK_VALUE * __builtin_popcountll(board[0][3]);
    score -= QUEEN_VALUE * __builtin_popcountll(board[0][4]);
    // score += rand() % 100;
    return side ? score : -score;
}

bool iterate_moves(int, int, int, U64[2][6], U64[2], int, char[3], char[5], int, int, U64, int*, char[6], int, int,int);
int alpha_beta(int, int, int, U64[2][6], U64[2], int, char*, char*, int, U64);

bool iterate_moves(int alpha, int beta, int depth, U64 board[2][6], U64 occupancy[2], 
               int side,char en_passant[3],char castling_rights[5], int ply,int from,U64 moves,int* best_score,char best_move[6],int piece,int captured_piece,int promoted_piece){
    
    char en_passant_copy[3];
    char castling_rights_copy[5];


    while (moves)
    {
        int square = __builtin_ctzll(moves);
        int move = (from << 6) | square ; 

        //Make move
        board[side][piece] &= ~(1ULL<<from);
        board[side][piece] |= 1ULL<<square;
        occupancy[side] &= ~(1ULL<<from);
        occupancy[side] |= 1ULL<<square;

        if(captured_piece != -1){
            board[1-side][captured_piece] &= ~(1ULL<<square);
            occupancy[1-side] &= ~(1ULL<<square);
        }
        if(promoted_piece != -1){
            board[side][0] &= ~(1ULL<<square);
            board[side][promoted_piece] |= 1ULL<<square;
        }

        strcpy(en_passant_copy,en_passant);
        strcpy(castling_rights_copy,castling_rights);

        if(piece == 5) { // King move
            if(side == 0) {
                castling_rights[0] = '-';
                castling_rights[1] = '-';
            } else {
                castling_rights[2] = '-';
                castling_rights[3] = '-';
            }
        } else if(piece == 3) { // Rook move
            if(from == 0) castling_rights[0] = '-';
            if(from == 7) castling_rights[1] = '-';
            if(from == 56) castling_rights[2] = '-';
            if(from == 63) castling_rights[3] = '-';
        }
        if(piece == 0) {
            // Double pawn push
            if(abs(from - square) == 16) {
                int ep_file = from % 8;
                en_passant[0] = 'a' + ep_file;
                en_passant[1] = side ? '2' : '5';
                en_passant[2] = '\0';
            }
        }

        en_passant[0] = '-';


        //score calculation
        int score;
        if(*best_score == -INFINITY){
            score = -alpha_beta(-beta, -alpha, depth-1, board, occupancy, 1-side,en_passant,castling_rights, ply+1, generate_hash(board, 1-side, en_passant));
        }else {
            score = -alpha_beta(-alpha-1, -alpha, depth-1, board, occupancy, 1-side,en_passant,castling_rights, ply+1, generate_hash(board, 1-side, en_passant));
            if(score > alpha && score < beta)
                score = -alpha_beta(-beta, -alpha, depth-1, board, occupancy, 1-side,en_passant,castling_rights, ply+1, generate_hash(board, 1-side, en_passant));
        }
        
        //Undo move
        if(promoted_piece != -1){
            board[side][0] |= 1ULL<<square;
            board[side][promoted_piece] &= ~(1ULL<<square);
        }
        board[side][piece] &= ~(1ULL<<square);
        board[side][piece] |= (1ULL<<from);
        occupancy[side] &= ~(1ULL<<square);
        occupancy[side] |= (1ULL<<from);

        if(captured_piece != -1){
            board[1-side][captured_piece] |= (1ULL<<square);
            occupancy[1-side] |= (1ULL<<square);
        }

        strcpy(en_passant,en_passant_copy);
        strcpy(castling_rights,castling_rights_copy);
        
        //better move found
        if(score > *best_score) {
            *best_score = score;
            char output_square[3];
            char output_from[3];
            square_to_chess(from,output_from);
            square_to_chess(square,output_square);
            if(promoted_piece!=-1){
                sprintf(best_move,"%s%s%c",output_from,output_square,piece_chars[promoted_piece]);
            }else{
                sprintf(best_move,"%s%s",output_from,output_square);
            }
            
            // Update PV table
            strncpy(pv_table[ply][ply] , best_move,6);
            for(int next_ply = ply + 1; next_ply < pv_length[ply + 1]; next_ply++)
                strncpy(pv_table[ply][next_ply] , pv_table[ply + 1][next_ply] , 6);
            pv_length[ply] = pv_length[ply + 1];
        }

        //pruned
        alpha = max(alpha, score);
        if(alpha >= beta) {
            // Store killer move
            if(captured_piece==-1) {
                killer_moves[ply][1] = killer_moves[ply][0];
                killer_moves[ply][0] = move;
            }
            
            // Update history heuristic
            // if(captured_piece == -1)
            //     history[side][from][square] += depth * depth;
            
           return true;
        }

        moves &= moves-1;
    }
    return false;
}

// Alpha-beta search with PV
int alpha_beta(int alpha, int beta, int depth, U64 board[2][6], U64 occupancy[2], 
               int side,char* en_passant,char *castling_rights, int ply, U64 hash) {

    if(depth <= 0)
        // return quiescence(alpha, beta, board, occupancy, side,en_passant,castling_rights);
        // printu64(occupancy[0]|occupancy[1]);
        // printf("%d\n",evaluate(board, side));
        return evaluate(board, side);
    
    pv_length[ply] = ply;
    
    // Transposition table lookup with null check
    TTEntry *tt_entry = &tt[hash % TT_SIZE];
    if (tt_entry && tt_entry->key == hash && tt_entry->depth >= depth) {
        if(tt_entry->flag == HASH_EXACT)
            return tt_entry->score;
        if(tt_entry->flag == HASH_ALPHA && tt_entry->score <= alpha)
            return alpha;
        if(tt_entry->flag == HASH_BETA && tt_entry->score >= beta)
            return beta;
    }
    
    PieceMoves pieces[6]={0};
    PieceMoves opponent_pieces[6]={0};
    int8_t control[64]={0};
    U64 own_control=0,opponent_control=0,is_attacked=0;

    for(int piece = 0; piece < 6; piece++) {
        pieces[piece] = initialize_moves(board, occupancy[side], occupancy[1-side], piece, side);

        for(int i=0 ; pieces[piece].attacks[i] != 0 ; i++){
            if(pieces[piece].square[i]>63){
                printf("%d %d %d\n",piece,i,pieces[piece].square[i]);
                printu64(occupancy[0]);
                printu64(occupancy[1]);
            }
            U64 attack = pieces[piece].attacks[i];
            while(attack){
                control[__builtin_ctzll(attack)]++;
                attack &= attack -1 ;
            }
        }
        opponent_pieces[piece] = initialize_moves(board, occupancy[1-side], occupancy[side], piece, 1-side);
        for(int i=0 ; opponent_pieces[piece].attacks[i] != 0 ; i++){
            U64 attack = opponent_pieces[piece].attacks[i];
            is_attacked |= attack;

            while(attack){
                control[__builtin_ctzll(attack)]--;
                attack &= attack -1 ;
            }
        }
    }
    
    
    for (int i = 0; i < 64; i++)
    {
        if(control[i]>0){
            own_control |= (1ULL<<i); 
        }else if(control[i]<0){
            opponent_control |= (1ULL<<i);
        }
    }
    
    
    int best_score = -INFINITY;
    char best_move[6] = "";
    int old_alpha = alpha;

    int opponent_king_square = opponent_pieces[5].square[0];
    U64 piece_checks[5];
    piece_checks[0] = pawn_attacks(opponent_king_square,1-side);
    piece_checks[1] = knight_attacks(opponent_king_square);
    piece_checks[2] = bishop_attacks(opponent_king_square,occupancy[side],occupancy[1-side],0);
    piece_checks[3] = rook_attacks(opponent_king_square,occupancy[side],occupancy[1-side],0);
    piece_checks[4] = piece_checks[2] | piece_checks[3];
    
    //able to capture king
    for(int piece=0;piece<6;piece++){
        for(int i=0;pieces[piece].attacks[i] && i<8;i++){
            if(pieces[piece].moves[i] & board[1-side][5]){
                // if(piece ==0 && pieces[piece].square[i]==0){
                //     printu64(occupancy[0]);
                //     printu64(occupancy[1]);
                // }
                // printf("%d %d \n",piece,pieces[piece].square[i]);
                // printu64(pieces[piece].moves[i]);
                // printu64(pieces[piece].attacks[i]);
                // printu64(board[1-side][5]);
                return INFINITY-ply;
            }
        }
    }

    // promotion - queen
    for(int i=0;i<8;i++){
        if(pieces[0].moves[i] & 18374686479671623680ULL){
            for(int j=4;j>=0;j--){
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[0].square[i],pieces[0].moves[i] & board[1-side][j],&best_score,best_move,0,j,4)){
                    goto beta_cutoff;
                }
                pieces[0].moves[i] &= ~(board[1-side][j]);
            }
            if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[0].square[i],pieces[0].moves[i],&best_score,best_move,0,-1,4)){
                goto beta_cutoff;
            }
        }
    }
    
    // checks with contol captures
    for(int j=4;j>=0;j--){
        for(int piece=4;piece>=0;piece--){
            for(int i=0;pieces[piece].attacks[i] && i<8;i++){
                U64 moves = pieces[piece].moves[i] & piece_checks[piece] & own_control & occupancy[1-side];
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],moves & board[1-side][j],&best_score,best_move,piece,j,-1)){
                    goto beta_cutoff;
                }
                pieces[piece].moves[i] &= ~(moves & board[1-side][j]);
            }
        }
    }

    // checks with safe square
    for(int piece=4;piece>=0;piece--){
        for(int i=0;pieces[piece].attacks[i] && i<8;i++){
            if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],pieces[piece].moves[i] & piece_checks[piece] & own_control,&best_score,best_move,piece,-1,-1)){
                goto beta_cutoff;
            }
            pieces[piece].moves[i] &= ~(piece_checks[piece] & own_control);

        }
    }
    // contol captures
    for(int j=4;j>=0;j--){
        for(int piece=5;piece>=0;piece--){
            for(int i=0;pieces[piece].attacks[i] && i<8;i++){
                U64 moves = pieces[piece].moves[i] & own_control & occupancy[1-side];
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],moves & board[1-side][j],&best_score,best_move,piece,j,-1)){
                    goto beta_cutoff;
                }
                pieces[piece].moves[i] &= ~(moves & board[1-side][j]);
            }
        }
    }
    // advantageous captures
    for(int piece=0;piece<5;piece++){
        for(int j=4;j>=piece;j--){
            for(int i=0;pieces[piece].attacks[i] && i<8;i++){
                U64 moves = pieces[piece].moves[i] & occupancy[1-side];
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],moves & board[1-side][j],&best_score,best_move,piece,j,-1)){
                    goto beta_cutoff;
                }
                pieces[piece].moves[i] &= ~(moves & board[1-side][j]);
            }
        }
    }

    // promotion - other
    for(int i=0;i<8;i++){
        if(pieces[0].moves[i] & 18374686479671623680ULL){
            for(int k=3;k>0;k--){
                for(int j=4;j>=0;j--){
                    if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[0].square[i],pieces[0].moves[i] & board[1-side][j],&best_score,best_move,0,j,k)){
                        goto beta_cutoff;
                    }
                    pieces[0].moves[i] &= ~(board[1-side][j]);
                }
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[0].square[i],pieces[0].moves[i],&best_score,best_move,0,-1,k)){
                    goto beta_cutoff;
                }
            }
        }
    }
    // // enpassant
    if(en_passant[0] != '-'){
        int square = (int)(en_passant[0]-'a')*8 + (int)(en_passant[1]-'1');
        
        if(side){
            if((board[1][0] & (1ULL<<(square-7))) && (square % 8 != 7)){
                board[0][0] &= ~(1ULL<<(square-8));
                occupancy[0] &= ~(1ULL<<(square-8));
                if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,square-7, (1ULL<<square),&best_score,best_move,0,-1,-1)){
                    board[0][0] |= (1ULL<<(square-8));
                    occupancy[0] |= (1ULL<<(square-8));
                    goto beta_cutoff;
                }
                board[0][0] |= (1ULL<<(square-8));
                occupancy[0] |= (1ULL<<(square-8));
            }
            if((board[1][0] & (1ULL<<(square-9))) && (square % 8 != 0)){
                board[0][0] &= ~(1ULL<<(square-8));
                occupancy[0] &= ~(1ULL<<(square-8));
                if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,square-9, (1ULL<<square),&best_score,best_move,0,-1,-1)){
                    board[0][0] |= (1ULL<<(square-8));
                    occupancy[0] |= (1ULL<<(square-8));
                    goto beta_cutoff;
                }
                board[0][0] |= (1ULL<<(square-8));
                occupancy[0] |= (1ULL<<(square-8));
            }
        }else{
            if((board[0][0] & (1ULL<<(square+7))) && (square & 7 != 0)){
                board[0][0] &= ~(1ULL<<(square+8));
                occupancy[0] &= ~(1ULL<<(square+8));
                if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,square+7, (1ULL<<square),&best_score,best_move,0,-1,-1)){
                    board[0][0] |= (1ULL<<(square-8));
                    occupancy[0] |= (1ULL<<(square-8));
                    goto beta_cutoff;
                }
                board[0][0] |= (1ULL<<(square-8));
                occupancy[0] |= (1ULL<<(square-8));
            }
            if((board[0][0] & (1ULL<<(square+9))) && (square & 7 != 7)){
                board[0][0] &= ~(1ULL<<(square+8));
                occupancy[0] &= ~(1ULL<<(square+8));
                if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,square+9, (1ULL<<square),&best_score,best_move,0,-1,-1)){
                    board[0][0] |= (1ULL<<(square-8));
                    occupancy[0] |= (1ULL<<(square-8));
                    goto beta_cutoff;
                }
                board[0][0] |= (1ULL<<(square-8));
                occupancy[0] |= (1ULL<<(square-8));
            }
        }
        
    }
    // castling
    if(side){
        if(castling_rights[0]=='K' && !(is_attacked & 112ULL) && (occupancy[0] & occupancy[1] & 48ULL)){
            board[1][3] &= ~(1ULL<<7);
            occupancy[1] &= ~(1ULL<<7);
            board[1][3] |= (1ULL<<5);
            occupancy[1] |= (1ULL<<5);
            if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,4, 64ULL,&best_score,best_move,5,-1,-1)){
                board[1][3] |= (1ULL<<7);
                occupancy[1] |= (1ULL<<7);
                board[1][3] &= ~(1ULL<<5);
                occupancy[1] &= ~(1ULL<<5);
                goto beta_cutoff;
            }
            board[1][3] |= (1ULL<<7);
            occupancy[1] |= (1ULL<<7);
            board[1][3] &= ~(1ULL<<5);
            occupancy[1] &= ~(1ULL<<5);
        }
        if(castling_rights[1]=='Q' && !(is_attacked & 30ULL) && (occupancy[0] & occupancy[1] & 14ULL)){
            board[1][3] &= ~(1ULL);
            occupancy[1] &= ~(1ULL);
            board[1][3] |= (1ULL<<3);
            occupancy[1] |= (1ULL<<3);
            if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,4, 4ULL,&best_score,best_move,5,-1,-1)){
                board[1][3] |= (1ULL);
                occupancy[1] |= (1ULL);
                board[1][3] &= ~(1ULL<<3);
                occupancy[1] &= ~(1ULL<<3);
                goto beta_cutoff;
            }
            board[1][3] |= (1ULL);
            occupancy[1] |= (1ULL);
            board[1][3] &= ~(1ULL<<3);
            occupancy[1] &= ~(1ULL<<3);
        }
    }else{
        if(castling_rights[2]=='k' && !(is_attacked & 8070450532247928832ULL) && (occupancy[0] & occupancy[1] & 6917529027641081856ULL)){
            board[0][3] &= ~(1ULL<<63);
            occupancy[0] &= ~(1ULL<<63);
            board[0][3] |= (1ULL<<61);
            occupancy[0] |= (1ULL<<61);
            if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,60 , 4611686018427387904ULL,&best_score,best_move,5,-1,-1)){
                board[1][3] |= (1ULL<<63);
                occupancy[1] |= (1ULL<<63);
                board[1][3] &= ~(1ULL<<61);
                occupancy[1] &= ~(1ULL<<61);
                goto beta_cutoff;
            }
            board[1][3] |= (1ULL<<63);
            occupancy[1] |= (1ULL<<63);
            board[1][3] &= ~(1ULL<<61);
            occupancy[1] &= ~(1ULL<<61);
        }
        if(castling_rights[3]=='q' && !(is_attacked & 2161727821137838080ULL) && (occupancy[0] & occupancy[1] & 1008806316530991104ULL)){
            board[0][3] &= ~(1ULL<<56);
            occupancy[0] &= ~(1ULL<<56);
            board[0][3] |= (1ULL<<59);
            occupancy[0] |= (1ULL<<59);
            if (iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,60 , 288230376151711744ULL,&best_score,best_move,5,-1,-1)){
                board[1][3] |= (1ULL<<56);
                occupancy[1] |= (1ULL<<56);
                board[1][3] &= ~(1ULL<<59);
                occupancy[1] &= ~(1ULL<<59);
                goto beta_cutoff;
            }
            board[1][3] |= (1ULL<<56);
            occupancy[1] |= (1ULL<<56);
            board[1][3] &= ~(1ULL<<59);
            occupancy[1] &= ~(1ULL<<59);
        }
    }

    // quiet without hanging
    for(int piece=5;piece>0;piece--){
        for(int i=0;pieces[piece].attacks[i] && i<8;i++){
            if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],pieces[piece].moves[i] & own_control,&best_score,best_move,piece,-1,-1)){
                goto beta_cutoff;
            }
            pieces[piece].moves[i] &= ~(own_control);
        }
    }
    for(int i=0;pieces[0].attacks[i] && i<8;i++){
        if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[0].square[i],pieces[0].moves[i] & ~(opponent_control),&best_score,best_move,0,-1,-1)){
            goto beta_cutoff;
        }
        pieces[0].moves[i] &= opponent_control;
    }
    // checks with hanging/bad captures
    for(int j=4;j>=0;j--){
        for(int piece=4;piece>=0;piece--){
            for(int i=0;pieces[piece].attacks[i] && i<8;i++){
                U64 moves = pieces[piece].moves[i] & piece_checks[piece];
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],moves & board[1-side][j],&best_score,best_move,piece,j,-1)){
                    goto beta_cutoff;
                }
                pieces[piece].moves[i] &= ~(moves & board[1-side][j]);
            }
        }
    }
    // bad captures
    for(int piece=1;piece<5;piece++){
        for(int j=piece-1;j>=0;j--){
            for(int i=0;pieces[piece].attacks[i] && i<8;i++){
                U64 moves = pieces[piece].moves[i] & occupancy[1-side];
                if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],moves & board[1-side][j],&best_score,best_move,piece,j,-1)){
                    goto beta_cutoff;
                }
                pieces[piece].moves[i] &= ~(moves & board[1-side][j]);
            }
        }
    }
    // hanging moves
    for(int piece=4;piece>=0;piece--){
        for(int i=0;pieces[piece].attacks[i] && i<8;i++){
            if(iterate_moves(alpha, beta, depth, board, occupancy, side,en_passant,castling_rights, ply,pieces[piece].square[i],pieces[piece].moves[i],&best_score,best_move,piece,-1,-1)){
                goto beta_cutoff;
            }
        }
    }

beta_cutoff:
    // Store position in transposition table
    tt_entry->key = hash;
    tt_entry->score = best_score;
    tt_entry->depth = depth;
    strncpy(tt_entry->best_move , best_move , 6);
    if(best_score <= old_alpha)
        tt_entry->flag = HASH_ALPHA;
    else if(best_score >= beta)
        tt_entry->flag = HASH_BETA;
    else
        tt_entry->flag = HASH_EXACT;
    
    return best_score;
}

// Iterative deepening search
void iterative_deepening(U64 board[2][6], U64 occupancy[2], int side,char* en_passant,char *castling_rights, int max_depth,char best_move[6]) {

    for(int depth = 1; depth <= max_depth; depth++) {
        int score = alpha_beta(-INFINITY, INFINITY, depth, board, occupancy, side,en_passant,castling_rights, 0, generate_hash(board, side,en_passant));
        
        // Time management would go here
        
        strncpy(best_move , pv_table[0][0] , 6);
        
        // Print info (depth, score, PV line, etc.)
        // printf("info depth %d score cp %d pv", depth, score);
        // for(int i = 0; i < pv_length[0]; i++) {
        //     printf(" %s ",pv_table[0][i]);
        //     // printf(" %s%s", square_to_string(move >> 6), square_to_string(move & 0x3F));
        // }
        // printf("\n");
    }
}


void initialize(int summa){
    init_hash_keys();
}

char final_move[6];

char * engine(char *fen,int depth){
    reset_pv();
    U64 board[2][6] = {0};
    U64 occupancy[2] = {0};
    char active_color;
    char castling_rights[5];
    char en_passant[3];
    int halfmove_clock;
    int fullmove_number;
    parse_fen(fen, board, occupancy, &active_color, castling_rights, en_passant, &halfmove_clock, &fullmove_number);
    iterative_deepening(board, occupancy, (active_color == 'w') ? 1 : 0,en_passant,castling_rights, 4,final_move);
    return final_move;
}

// int main(){
//     // Initialize hash keys
//     init_hash_keys();
//     // Initialize board
//     reset_pv();
//     U64 board[2][6] = {0};
//     U64 occupancy[2] = {0};
//     char fen[] = "rnbqkb1r/ppp2pPp/3p1n2/8/4p3/8/PPPPPPP1/RNBQKBNR w KQkq - 1 5";
//     char active_color;
//     char castling_rights[5];
//     char en_passant[3];
//     int halfmove_clock;
//     int fullmove_number;
//     parse_fen(fen, board, occupancy, &active_color, castling_rights, en_passant, &halfmove_clock, &fullmove_number);

//     // Start search
//     char best_move[6];
//     iterative_deepening(board, occupancy, (active_color == 'w') ? 1 : 0,en_passant,castling_rights, 4,best_move);
//     printf("Best move: %s\n", best_move);
//     // PieceMoves pieces[6]={0};

//     // for(int piece = 0; piece < 6; piece++) {
//     //     pieces[piece] = initialize_moves(board, occupancy[0], occupancy[1], piece, 0);
//     // }
//     // for(int i=0;i<6;i++){
//     //     for(int j=0;pieces[i].attacks[j] != 0;j++){
//     //         printf("%d\n",pieces[i].square[j]);
//     //         printu64(pieces[i].moves[j]);
//     //     }
//     // }

//     return 0;
// }