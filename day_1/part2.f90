program part1
    ! now we want cals held by top 3 elves
    implicit none

    integer :: readcals
    integer :: heldcals
    integer :: maxcals
    integer :: counter
    integer :: big_holder(3)
    character(len=200) :: line
    integer :: io
    integer :: stat

    counter = 0
    maxcals = 0
    big_holder = [0, 0, 0]
    readcals = 0
    heldcals = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file
    do 
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop
        counter = counter + 1 ! increment elf counter
        if (len_trim(adjustl(line)) > 0 ) then ! check that the character string can become an int
            read(line,'(I5)') readcals ! make the char string an in
            heldcals = heldcals + readcals ! increment cals held by elf
        else
            ! if we hit a blank line, tally up and check if we have the most cals
            ! check if the current holder has more than the first holder (max)
            
            if (big_holder(1) < heldcals) then
                ! bump everything down
                big_holder(3) = big_holder(2)
                big_holder(2) = big_holder(1)
                big_holder(1) = heldcals
            else if (big_holder(2) < heldcals) then
                big_holder(3) = big_holder(2)
                big_holder(2) = heldcals
            else if (big_holder(3) < heldcals) then
                big_holder(3) = heldcals
            end if
            print *, big_holder
            heldcals = 0 ! reset held cals counter
        end if
        
    end do

    print *, "top 3 sum: ", sum(big_holder)
endprogram