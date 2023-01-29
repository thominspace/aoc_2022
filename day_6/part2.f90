function check_repeates(buffer)
    character(len=14) :: buffer
    integer :: ix, iy

    check_repeates = 0

    ! check every character against every forward character
    ! maybe not most efficient? I'd do this in a dict in python but oh well
    checkloop: do ix = 1, 13
        do iy = ix+1, 14
            if (buffer(ix:ix).eq.buffer(iy:iy))then    
                check_repeates = 1
                exit checkloop
            end if
        end do
    end do checkloop

end function check_repeates

program part2
    ! WELP this is where bad coding bites me in the ass
    implicit none

    character(len=8000) :: line
    character(len=14) :: buffer
    integer :: io
    integer :: stat
    integer :: counter
    integer :: ix, iy
    real :: check_repeates

    counter = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file

    checkloop : do
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop

        ! we expect the whole line on the first go this time

        ! can prefill first 14 chracters
        do ix = 1, 14
            buffer(ix:ix) = line(ix:ix)
        end do
        counter = 4

        if (check_repeates(buffer).eq.0) exit checkloop
        do ix = 5, len(line)
            ! shuffle characters down
            do iy = 1, 13
                buffer(iy:iy) = buffer(iy+1:iy+1)
            end do

            ! get character
            buffer(14:14) = line(ix:ix)
            counter = counter + 1

            ! check repeates
            if (check_repeates(buffer).eq.0) exit checkloop
        end do
        
    end do checkloop
    print *, "counter: ", counter
endprogram