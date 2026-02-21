program main
    use iso_c_binding
    implicit none

    interface
        function gzopen(path, mode) bind(c, name="gzopen")
            import :: c_ptr, c_char
            type(c_ptr) :: gzopen
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(in) :: mode(*)
        end function
        
        function gzread(file, buf, len) bind(c, name="gzread")
            import :: c_ptr, c_int
            integer(c_int) :: gzread
            type(c_ptr), value :: file
            type(c_ptr), value :: buf
            integer(c_int), value :: len
        end function
        
        function gzclose(file) bind(c, name="gzclose")
            import :: c_ptr, c_int
            integer(c_int) :: gzclose
            type(c_ptr), value :: file
        end function

        function gzwrite(file, buf, len) bind(c, name="gzwrite")
            import :: c_ptr, c_int
            integer(c_int) :: gzwrite
            type(c_ptr), value :: file
            type(c_ptr), value :: buf
            integer(c_int), value :: len
        end function
    end interface
    
    integer :: i, ios, file_size, arg_count
    character(len=256) :: inputFile, search_name,GMOFile
    integer, parameter :: xor_key = 11
    type(c_ptr) :: file_ptr
    integer :: bytes_read
    character(len=:), allocatable :: raw_data, binary_gz_data

    arg_count = command_argument_count()
    if (arg_count < 3) stop "Usage: .\main.exe <path_to_dat> <level_name> <GMO File>"
    
    call get_command_argument(1, inputFile)
    call get_command_argument(2, search_name)
    call get_command_argument(3, GMOFile)

    inquire(file=trim(inputFile), size=file_size, iostat=ios)
    if (ios /= 0) stop 'FILE NOT FOUND: ' // trim(inputFile)

    allocate(character(len=file_size) :: raw_data)
    open(unit=10, file=inputFile, access='stream', status='old')
    read(10) raw_data
    close(10)

    do i = 1, file_size
        raw_data(i:i) = char(ieor(ichar(raw_data(i:i)), xor_key))
        if (raw_data(i:i) == '-') raw_data(i:i) = '+'
        if (raw_data(i:i) == '_') raw_data(i:i) = '/'
    end do

    binary_gz_data = base64_decode(raw_data)
    deallocate(raw_data) 

    open(unit=13, file='temp_save.gz', access='stream', status='replace')
    write(13) binary_gz_data
    close(13)

    file_ptr = gzopen("temp_save.gz" // c_null_char, "rb" // c_null_char)
    block
        character(len=:), allocatable, target :: big_buffer 
        integer :: pos_name, pos_ks4, pos_s_start, pos_s_end, obj_count
        character(len=:), allocatable :: level_data_raw
        type(c_ptr) :: lvl_file_ptr 
        integer(c_int) :: est_size = 100000000_c_int 

        allocate(character(len=est_size) :: big_buffer)
        bytes_read = gzread(file_ptr, c_loc(big_buffer(1:1)), est_size)
        i = gzclose(file_ptr)
        
        pos_name = index(big_buffer(1:bytes_read), "<k>kS1</k><s>"//trim(search_name)//"</s>")
        if (pos_name == 0) pos_name = index(big_buffer, "<s>"//trim(search_name)//"</s>")
        
        if (pos_name > 0) then
            print *, "Found level: ", trim(search_name)
            pos_ks4 = index(big_buffer(pos_name:), "<k>kS4</k>") + pos_name - 1
            pos_s_start = index(big_buffer(pos_ks4 + 10:), "<s>") + (pos_ks4 + 10) + 2
            pos_s_end = index(big_buffer(pos_s_start:), "</s>") + pos_s_start - 2
            
            level_data_raw = big_buffer(pos_s_start:pos_s_end)
            do i = 1, len(level_data_raw)
                if (level_data_raw(i:i) == '-') level_data_raw(i:i) = '+'
                if (level_data_raw(i:i) == '_') level_data_raw(i:i) = '/'
            end do

            binary_gz_data = base64_decode(level_data_raw)

            i = index(binary_gz_data, char(31)//char(139)) 
            if (i > 0) then
                open(unit=14, file='temp_lvl.gz', access='stream', status='replace')
                write(14) binary_gz_data(i:)
                close(14)
            end if

            lvl_file_ptr = gzopen("temp_lvl.gz" // c_null_char, "rb" // c_null_char)
            if (c_associated(lvl_file_ptr)) then
                block
                    character(len=:), allocatable, target :: lvl_content
                    integer :: br, start_ptr, end_ptr
                    allocate(character(len=20000000) :: lvl_content) 

                    br = gzread(lvl_file_ptr, c_loc(lvl_content), 20000000_c_int)
                    i = gzclose(lvl_file_ptr)
                    
                    obj_count = 0
                    start_ptr = index(lvl_content, ";") + 1
                    do i = start_ptr, br
                        if (lvl_content(i:i) == ';') then
                            obj_count = obj_count + 1
                        end if
                    end do
                    print *, "Level uncompressed. Objects found: ", obj_count
                    block
                        character(len=:), allocatable, target :: modded_content
                        character(len=256) :: temp_obj, current_obj_str
                        integer :: start_ptr, end_ptr, x, y, obj_id
                        integer :: head_end, shift_x
                        type(c_ptr) :: write_ptr
                        
                        head_end = index(lvl_content, ";")
                        modded_content = lvl_content(1:head_end)

                        start_ptr = head_end + 1
                        shift_x = 0  
                        
                        do i = start_ptr, br
                            if (lvl_content(i:i) == ';') then
                                end_ptr = i
                                current_obj_str = lvl_content(start_ptr:end_ptr-1)
           
                                if (index(current_obj_str, "1,8,") == 0) then
                                    modded_content = modded_content // trim(current_obj_str) // ";"
                                end if
                                
                                start_ptr = i + 1
                            end if
                        end do
                        print *, "Injecting from file "//GMOFile
                        open(unit=30, file=GMOFile, status='old')
                        do
                            read(30, '(A)', iostat=ios) temp_obj
                            if (ios /= 0) exit
                            modded_content = modded_content // trim(temp_obj)
                        end do
                        close(30)

                        write_ptr = gzopen("modded_level.gz" // c_null_char, "wb" // c_null_char)
                        if (c_associated(write_ptr)) then
                            i = gzwrite(write_ptr, c_loc(modded_content), int(len(modded_content), c_int))
                            i = gzclose(write_ptr)
                            print *, "Step 1: New level data compressed to modded_level.gz"
                        end if
                    end block
                    print *, "Modification complete. Saving..."

                
               block
                    character(len=:), allocatable, target :: encoded_lvl, final_full_xml
                    character(len=:), allocatable, target :: mod_gz_data
                    character(len=:), allocatable :: final_gz_data  
                    character(len=:), allocatable :: final_b64      
                    integer :: mod_gz_size, total_len, final_gz_size 
                    type(c_ptr) :: final_gz_ptr

                    inquire(file='modded_level.gz', size=mod_gz_size)
                    allocate(character(len=mod_gz_size) :: mod_gz_data)
                    open(unit=20, file='modded_level.gz', access='stream', status='old')
                    read(20) mod_gz_data
                    close(20)
                

                    call base64_encode_sub(mod_gz_data, encoded_lvl)
                    deallocate(mod_gz_data)
                    
                    total_len = (pos_s_start - 1) + len(encoded_lvl) + (bytes_read - pos_s_end)
                    allocate(character(len=total_len) :: final_full_xml)
                  
                    final_full_xml(1 : pos_s_start-1) = big_buffer(1 : pos_s_start-1)
                    final_full_xml(pos_s_start : pos_s_start + len(encoded_lvl) - 1) = encoded_lvl
                    final_full_xml(pos_s_start + len(encoded_lvl) : total_len) = &
                        big_buffer(pos_s_end + 1 : bytes_read)
                    
                    print *, "Step 2: New XML constructed. Size:", total_len
                    deallocate(encoded_lvl) 

                    final_gz_ptr = gzopen("final_save.gz" // c_null_char, "wb" // c_null_char)
                    if (c_associated(final_gz_ptr)) then
                        i = gzwrite(final_gz_ptr, c_loc(final_full_xml(1:1)), int(len(final_full_xml), c_int))
                        i = gzclose(final_gz_ptr)
                        print *, "Step 3: Save XML compressed to final_save.gz"
                    end if

                    inquire(file='final_save.gz', size=final_gz_size)
                    allocate(character(len=final_gz_size) :: final_gz_data) 
                    open(unit=21, file='final_save.gz', access='stream', status='old')
                    read(21) final_gz_data
                    close(21)

                    call base64_encode_sub(final_gz_data, final_b64)
                    deallocate(final_gz_data)
                    print *, "Step 4: Applying XOR encryption..."
                    do i = 1, len(final_b64)
                        final_b64(i:i) = char(ieor(ichar(final_b64(i:i)), xor_key))
                    end do

                    open(unit=22, file='CCLocalLevels_MOD.dat', access='stream', status='replace')
                    write(22) final_b64
                    close(22)

                    print *, "--- SUCCESS ---"
                    print *, "Created: CCLocalLevels_MOD.dat"
                end block
                    
                end block

        
     
         end if
        else
            print *, "Level not found: ", trim(search_name)
        end if
    end block

    contains

    function base64_decode(input) result(output)
        character(len=*), intent(in) :: input
        character(len=(len(input)*3)/4) :: output
        character(len=64), parameter :: B64 = &
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        integer :: i, j, k, v(4), count
        character :: c
        output = ""
        j = 1
        count = 0
        do i = 1, len(input)
            c = input(i:i)
            if (c == "=" .or. c == char(13) .or. c == char(10) .or. c == " ") cycle
            
            k = index(B64, c)
            if (k <= 0) cycle
            
            count = count + 1
            v(count) = k - 1
            
            if (count == 4) then
                output(j:j) = char(ior(ishft(v(1), 2), ishft(v(2), -4)))
                output(j+1:j+1) = char(ior(ishft(and(v(2), 15), 4), ishft(v(3), -2)))
                output(j+2:j+2) = char(ior(ishft(and(v(3), 3), 6), v(4)))
                j = j + 3
                count = 0
            end if
        end do
        if (count == 2) then
            output(j:j) = char(ior(ishft(v(1), 2), ishft(v(2), -4)))
        else if (count == 3) then
            output(j:j) = char(ior(ishft(v(1), 2), ishft(v(2), -4)))
            output(j+1:j+1) = char(ior(ishft(and(v(2), 15), 4), ishft(v(3), -2)))
        end if
    end function base64_decode
    
    function base64_encode(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        character(len=64), parameter :: B64 = &
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        integer :: i, n, triplet(3), b(4)
        character(len=4) :: chunk

        n = len(input)
        allocate(character(len=((n+2)/3)*4) :: output)
        output = ""
        
        do i = 1, n, 3
            triplet = 0
            triplet(1) = ichar(input(i:i))
            if (i+1 <= n) triplet(2) = ichar(input(i+1:i+1))
            if (i+2 <= n) triplet(3) = ichar(input(i+2:i+2))
            
            b(1) = ishft(triplet(1), -2)
            b(2) = ior(ishft(and(triplet(1), 3), 4), ishft(triplet(2), -4))
            b(3) = ior(ishft(and(triplet(2), 15), 2), ishft(triplet(3), -6))
            b(4) = and(triplet(3), 63)

            chunk(1:1) = B64(b(1)+1:b(1)+1)
            chunk(2:2) = B64(b(2)+1:b(2)+1)
            
            if (i+1 <= n) then
                chunk(3:3) = B64(b(3)+1:b(3)+1)
            else
                chunk(3:3) = "="
            end if
            
            if (i+2 <= n) then
                chunk(4:4) = B64(b(4)+1:b(4)+1)
            else
                chunk(4:4) = "="
            end if
            
            output(((i-1)/3)*4+1 : ((i-1)/3)*4+4) = chunk
        end do
        do i = 1, len(output)
            if (output(i:i) == '+') output(i:i) = '-'
            if (output(i:i) == '/') output(i:i) = '_'
        end do
    end function base64_encode

    subroutine base64_encode_sub(input, output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=64), parameter :: B64 = &
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
        integer :: i, n, triplet(3), b(4), out_idx
        character(len=4) :: chunk
        n = len(input)
     
        allocate(character(len=((n+2)/3)*4) :: output)            
        out_idx = 1
        do i = 1, n, 3
                triplet = 0
                triplet(1) = ichar(input(i:i))
                if (i+1 <= n) triplet(2) = ichar(input(i+1:i+1))
                if (i+2 <= n) triplet(3) = ichar(input(i+2:i+2))
                
                b(1) = ishft(triplet(1), -2)
                b(2) = ior(ishft(and(triplet(1), 3), 4), ishft(triplet(2), -4))
                b(3) = ior(ishft(and(triplet(2), 15), 2), ishft(triplet(3), -6))
                b(4) = and(triplet(3), 63)

                output(out_idx:out_idx) = B64(b(1)+1:b(1)+1)
                output(out_idx+1:out_idx+1) = B64(b(2)+1:b(2)+1)
                
                if (i+1 <= n) then
                    output(out_idx+2:out_idx+2) = B64(b(3)+1:b(3)+1)
                else
                    output(out_idx+2:out_idx+2) = "="
                end if
                
                if (i+2 <= n) then
                    output(out_idx+3:out_idx+3) = B64(b(4)+1:b(4)+1)
                else
                    output(out_idx+3:out_idx+3) = "="
                end if
                out_idx = out_idx + 4
     end do
    end subroutine base64_encode_sub

end program main
