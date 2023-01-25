program part1
    implicit none

    integer :: readcals
    integer :: heldcals
    integer :: maxcals
    integer :: counter
    integer :: big_holder
    character(len=200) :: line
    integer :: io
    integer :: stat

    counter = 0
    maxcals = 0
    big_holder = 0
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
            if (heldcals > maxcals) then 
                maxcals = heldcals
                big_holder = counter
            end if
            heldcals = 0 ! reset held cals counter
        end if
        
    end do

    print *, "Big holder: ", big_holder
    print *, "Has cals: ", maxcals
endprogram