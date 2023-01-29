subroutine printcratereg(crate_reg, crate_reg_len)
    character(len=1), dimension(9, 8*9) :: crate_reg
    integer, dimension(9) :: crate_reg_len
    integer :: ix, iy

    do ix = 1, 9
        ! do iy = 1, crate_reg_len(ix)
            print *, ix, "|", crate_reg(ix, :)
        ! end do
    end do

    return

end subroutine printcratereg

subroutine movecrate(crate_reg, crate_reg_len, move_num, from, to)
    character(len=1), dimension(9, 8*9) :: crate_reg
    character(len=200) :: temp_str
    integer, dimension(9) :: crate_reg_len
    integer :: from, to, ix

    temp_str = " "
    ! hold the crates to move
    do ix = 1, move_num
        temp_str(ix:ix) = crate_reg(from, crate_reg_len(from)-move_num+ix)
        crate_reg(from, crate_reg_len(from)-move_num+ix) = " "
    end do
    print *, "moving ", trim(temp_str), " from", from, "to", to
    print * , crate_reg_len

    ! "remove" the crate
    crate_reg_len(from) = crate_reg_len(from) - move_num

    ! "add" the crate

    do ix = 1, move_num        
        crate_reg(to, crate_reg_len(to)+1) = temp_str(ix:ix)
        crate_reg_len(to) = crate_reg_len(to) + 1
    end do
    call printcratereg(crate_reg, crate_reg_len)

    return

end subroutine movecrate

program part2
    ! same idea as part 1 but now we retain order
    implicit none
    character(len=200) :: line, temp_str
    character(len=1), dimension(9, 8*9) :: crate_reg
    integer, dimension(9) :: crate_reg_len
    integer :: io
    integer :: stat
    integer :: score
    integer :: line_ix, register_ix, ix, iy, char_pick_ix, split_ix, move_num, from_ix, to_ix

    score = 0
    ! do ix = 1, size(crate_reg_len)
    !     crate_reg_len(ix) = 0
    ! end do
    crate_reg_len = 0
    crate_reg = " "

    open(newunit=io, file="input.txt", status="old", action="read") ! open file

    ! need to make some sort of linked list (oh good god)

    ! read input
    ! length of crate input is known, so we can slice strings directly

    !     [B]             [B] [S]        
    !     [M]             [P] [L] [B] [J]
    !     [D]     [R]     [V] [D] [Q] [D]
    !     [T] [R] [Z]     [H] [H] [G] [C]
    !     [P] [W] [J] [B] [J] [F] [J] [S]
    ! [N] [S] [Z] [V] [M] [N] [Z] [F] [M]
    ! [W] [Z] [H] [D] [H] [G] [Q] [S] [W]
    ! [B] [L] [Q] [W] [S] [L] [J] [W] [Z]
    !  1   2   3   4   5   6   7   8   9 

    print *, crate_reg_len(:)
    print *, crate_reg(:,:)


    ! 8 crate lines
    do line_ix = 1, 8
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop
        print *, line

        ! load crate register with the charactes in the crates
        ! every 4 characters contains a crate, ever second character contains the character in question
        do register_ix = 1, 9
            char_pick_ix = register_ix*4-2
            print *, "--------"
            ! print *, char_pick_ix, line(char_pick_ix:char_pick_ix), (line(char_pick_ix:char_pick_ix).ne." ")
            if (line(char_pick_ix:char_pick_ix).ne." ") then
                crate_reg_len(register_ix) = crate_reg_len(register_ix) + 1
                crate_reg(register_ix, crate_reg_len(register_ix)) = line(char_pick_ix:char_pick_ix)
            end if
            print *, line(char_pick_ix:char_pick_ix)
            print *, crate_reg_len
            print *, crate_reg(:,:)
            print *, crate_reg(register_ix, crate_reg_len(register_ix))
        end do
    end do

    print *, crate_reg



    call printcratereg(crate_reg, crate_reg_len)
    print * , "~~~FLIP IT~~~"
    ! after loading, the columns need to be reversed
    ! print *, "**********"
    do ix = 1, 9 ! go through all columns
        ! print *, "current crate reg:", crate_reg(ix, :)
        temp_str = " "
        do iy = 1, crate_reg_len(ix) ! go through all boxes in column
            ! print *, "charselect ", crate_reg(ix, crate_reg_len(ix)-iy+1)
            temp_str(iy:iy) = crate_reg(ix, crate_reg_len(ix)-iy+1) ! copy in reverse
            ! print *, "tempstr ", temp_str
        end do
        ! print *, temp_str
        ! can't just copy, have to slice the strings
        do iy = 1, crate_reg_len(ix)
            crate_reg(ix, iy:iy) = temp_str(iy:iy) ! swap the flipped column in
        end do

        ! print *, "register is now ", crate_reg
        ! print *, "**********"
    end do
    call printcratereg(crate_reg, crate_reg_len)


    ! skip 2 index lines (this should probably be done with a cursor but oh well)
    do line_ix = 9, 10
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop
        ! print *, line
    end do


    ! read the rest
    do 
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop
        ! print *, line

        ! move 3 from 5 to 2
        ! split on spaces, slicing between first and second, third and fourth, and fifth and end
        temp_str = line
        split_ix = scan(temp_str, ' ')
        temp_str = temp_str(split_ix+1:len(temp_str)) ! drop the first word
        split_ix = scan(temp_str, ' ')
        read(temp_str(1:split_ix),'(I5)') move_num ! convert the second word to an int
        temp_str = temp_str(split_ix+1:len(temp_str)) ! drop the second word
        split_ix = scan(temp_str, ' ')
        temp_str = temp_str(split_ix+1:len(temp_str)) ! drop the third word
        split_ix = scan(temp_str, ' ')
        read(temp_str(1:split_ix),'(I5)') from_ix ! convert the fourth word to an int
        temp_str = temp_str(split_ix+1:len(temp_str)) ! drop the fourth word
        split_ix = scan(temp_str, ' ')
        temp_str = temp_str(split_ix+1:len(temp_str)) ! drop the fifth word
        split_ix = scan(temp_str, ' ')
        read(temp_str(1:split_ix),'(I5)') to_ix ! convert the sixth word to an int
        temp_str = temp_str(split_ix+1:len(temp_str)) ! drop the sixth word

        ! now do the moves
        print *, line, move_num, from_ix, to_ix
        call movecrate(crate_reg, crate_reg_len, move_num, from_ix, to_ix)
    end do

    ! answer the question ("whats on the top of each stack?")
    print *, "answer:"
    do ix = 1, 9
        print *, crate_reg(ix,crate_reg_len(ix))
    end do


endprogram