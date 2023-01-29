function check_repeates(c1, c2, c3, c4)
    character :: c1, c2, c3, c4

    check_repeates = 0

    if ((c1.eq.c2).or. &
        (c1.eq.c3).or. &
        (c1.eq.c4).or. &
        (c2.eq.c3).or. &
        (c2.eq.c4).or. &
        (c3.eq.c4)) then
    
        check_repeates = 1
    end if

end function check_repeates

program part1
    ! now we want cals held by top 3 elves
    implicit none

    character(len=8000) :: line
    character :: c1, c2, c3, c4 
    integer :: io
    integer :: stat
    integer :: counter
    integer :: ix
    real :: check_repeates

    counter = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file

    checkloop : do
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop

        ! we expect the whole line on the first go this time

        ! can prefill first 4 chracters
        c1 = line(1:1)
        c2 = line(2:2)
        c3 = line(3:3)
        c4 = line(4:4)
        counter = 4

        if (check_repeates(c1, c2, c3, c4).eq.0) exit checkloop
        do ix = 5, len(line)
            ! get character and shuffle down
            c1 = c2
            c2 = c3
            c3 = c4
            c4 = line(ix:ix)
            counter = counter + 1

            ! check repeates
            if (check_repeates(c1, c2, c3, c4).eq.0) exit checkloop
        end do
        
    end do checkloop
    print *, "counter: ", counter
endprogram