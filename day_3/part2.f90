program part2
    implicit none

    character(len=200) :: line1
    character(len=200) :: line2
    character(len=200) :: line3
    integer :: io
    integer :: stat
    integer :: score
    integer :: prio
    integer :: item
    logical :: found_dupe
    integer :: n1, n2, n3

    score = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file
    do 
        read(io, '(A)', iostat=stat) line1 ! read a line
        read(io, '(A)', iostat=stat) line2 ! read a line
        read(io, '(A)', iostat=stat) line3 ! read a line
        if (stat /= 0) exit ! if EOF exit do loop

        ! now need to find common character in all 3 lines
        found_dupe = .FALSE.

        ! check the characters in the first half against the second half
        search_bag : do while (found_dupe .eqv. .FALSE.)
            do n1 = 1, len(trim(line1))
                do n2 = 1, len(trim(line2))
                    if (line1(n1:n1) == line2(n2:n2)) then
                        ! found match, check next line
                        do n3 = 1, len(trim(line3))
                            if (line2(n2:n2) == line3(n3:n3)) then
                                found_dupe = .TRUE.
                                item = iachar(line1(n1:n1))
                                exit search_bag
                            end if
                        end do
                    end if
                end do
            end do
        end do search_bag

        ! 'a' starts at 97, we want it to have priority 1, so we can convert that to -96
        ! 'A' starts at 65 so we split and check upper or lower

        if (item>= iachar("a") .and. item<=iachar("z") ) then
            prio = item - 96
        else if (item>= iachar("A") .and. item<=iachar("Z") ) then
            prio = item - 64 + 26
        end if

        print *, line1, line2, line3, line1(n1:n1), prio

        score = score + prio
    end do
    print *, "score", score
endprogram
