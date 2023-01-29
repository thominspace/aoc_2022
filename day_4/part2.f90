program part2
    ! now we want any overlaps
    implicit none

    character(len=200) :: line, split1, split2, temp_char
    integer :: io
    integer :: stat
    integer :: score
    integer :: split_ix, left_min, left_max, right_min, right_max

    score = 0

    open(newunit=io, file="input.txt", status="old", action="read") ! open file
    do 
        read(io, '(A)', iostat=stat) line ! read a line
        if (stat /= 0) exit ! if EOF exit do loop
        ! print *, line

        ! split on comma
        split_ix = index(line, ',')
        split1 = line(1:split_ix-1)
        split2 = trim(line(split_ix+1:len(line)))

        ! split ranges on '-'
        split_ix = index(split1, '-')
        read(split1(1:split_ix-1),'(I5)') left_min
        read(split1(split_ix+1:len(split1)),'(I5)') left_max

        split_ix = index(split2, '-')
        read(split2(1:split_ix-1),'(I5)') right_min
        read(split2(split_ix+1:len(split2)),'(I5)') right_max

        ! check if one contains the other
        if (((left_min.ge.right_min).and.(left_min.le.right_max)).or. &
            ((right_min.ge.left_min).and.(right_min.le.left_max)).or. &
            ((left_max.ge.right_min).and.(left_max.le.right_max)).or. &
            ((right_max.ge.left_min).and.(right_max.le.left_max))) then
            score = score +1
            ! print *, "strings overlap", score
        end if

        ! print *, line, split1, split2, left_min, left_max, right_min, right_max
        
        ! print *, "--------------"
    end do
    print *, "score: ", score
endprogram