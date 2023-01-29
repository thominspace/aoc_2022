program part1
    ! now we want cals held by top 3 elves
    implicit none

    character(len=200) :: line
    integer :: io
    integer :: stat
    integer :: score
    integer :: prio
    integer :: item
    integer :: fulllen
    integer :: halflen
    logical :: found_dupe
    integer :: n
    integer :: m

    score = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file
    do 
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop

        ! need to check which characters appear in both halves of the string
        fulllen = len(trim(line))
        halflen = len(trim(line))/2
        found_dupe = .FALSE.

        ! check the characters in the first half against the second half
        search_bag : do while (found_dupe .eqv. .FALSE.)
            do n = 1, halflen
                do m = halflen+1, fulllen
                    if (line(n:n) == line(m:m)) then
                        found_dupe = .TRUE.
                        item = iachar(line(n:n))
                        exit search_bag
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

        print *, line, line(n:n), prio

        score = score + prio
    end do
    print *, "score", score
endprogram