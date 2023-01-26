program part1
    ! now we want cals held by top 3 elves
    implicit none

    character(len=200) :: line
    integer :: io
    integer :: stat
    integer :: score

    score = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file
    do 
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop
        ! print *, line

        ! make characters into points and calc
        ! The winner of the whole tournament is the player with the highest score. 
        ! Your total score is the sum of your scores for each round. 
        ! The score for a single round is the score for the shape you selected 
        ! (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the 
        ! outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

        ! A for Rock, B for Paper, and C for Scissors.
        ! X for Rock, Y for Paper, and Z for Scissors.

        if (line(1:1) == 'A') then ! opponent plays rock
            if (line(3:3) == 'X') then
                score = score + 4 ! tie (3) + rock (1)
            end if
            if (line(3:3) == 'Y') then
                score = score + 8 ! win (6) + paper (2)
            end if
            if (line(3:3) == 'Z') then
                score = score + 3 ! loss (0) + scissors (3)
            end if
        else if (line(1:1) == 'B') then ! opponent plays paper
            if (line(3:3) == 'X') then
                score = score + 1 ! loss (0) + rock (1)
            end if
            if (line(3:3) == 'Y') then
                score = score + 5 ! tie (3) + paper (2)
            end if
            if (line(3:3) == 'Z') then
                score = score + 9 ! win (6) + scissors (3)
            end if
        else if (line(1:1) == 'C') then ! opponent plays scissors
            if (line(3:3) == 'X') then
                score = score + 7 ! win (6) + rock (1)
            end if
            if (line(3:3) == 'Y') then
                score = score + 2 ! loss (0) + paper (2)
            end if
            if (line(3:3) == 'Z') then
                score = score + 6 ! tie (3) + scissors (3)
            end if
        end if




        print *, "total score: ", score
        
    end do
endprogram