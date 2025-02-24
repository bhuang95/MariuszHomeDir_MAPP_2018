program how_to_read_modis_hdf4

!----------------------------------------------------------------------------------------!

implicit none

integer i

character*300 file_name

integer DFACC_READ, DFNT_INT32

parameter ( DFACC_READ = 1, DFNT_INT32 = 24 )

integer sfstart, sfselect, sfrdata, sfendacc, sfend

integer sffinfo , sfginfo

integer sd_id, sds_id, sds_index

integer status, n_attrs

integer n_datasets, n_file_attrs , index

integer rank, data_type

integer MAX_NC_NAME , MAX_VAR_DIMS

parameter ( MAX_NC_NAME = 256 , MAX_VAR_DIMS = 32 )

character*( MAX_NC_NAME ) name

integer dim_sizes( MAX_VAR_DIMS )

character * 100 :: sds_name

integer start(3), edges(3), stride(3)

integer sfn2index

integer :: AllocateStatus, DeAllocateStatus

! data type 22 => integer 16 bits

integer*2, dimension(:,:), allocatable :: cloud_top_temperature_1km

integer sffattr, sfrattr, sfgainfo

integer      attr_index, n_values

character*20 attr_name

real(KIND=8) scale_factor

real(KIND=8) add_offset

! data type 20 => integer 8 bits

integer*1, dimension(:,:,:), allocatable :: Cloud_Mask_1km

integer cloud_mask_status_flag, cloud_mask

!----------------------------------------------------------------------------------------!

call getarg(1,file_name)

write(6,*) 'file_name : ' , file_name



!----------------------------------------------------------------------------------------!
! Get all SDS names

sd_id = sfstart(file_name, DFACC_READ)

status = sffinfo( sd_id , n_datasets , n_file_attrs )

write(6,*) '! Number of data sets in the file and Number of file attributes :'
write(6,*) 'sffinfo :', status , n_datasets , n_file_attrs
write(6,*)

do index = 0, n_datasets - 1

    sds_id = sfselect(sd_id, index)
    status = sfginfo(sds_id, name, rank, dim_sizes, data_type,n_attrs)

    write(6,*) "name : ", name(1:100), "rank : ", rank, &
             & "index : ", index, "dimension sizes are : ", (dim_sizes(i), i=1, rank), & 
             &  "data type is : ", data_type, "number of attributes is : ", n_attrs

end do

!----------------------------------------------------------------------------------------!
! Example of how to extract the data

index = sfn2index(sd_id, 'cloud_top_temperature_1km')

write(6,*) 'index', index

sds_id = sfselect(sd_id, index)
status = sfginfo( sds_id , name , rank , dim_sizes , data_type , n_attrs )

sds_name = name(1:len(name))

write(6,*) "index : ", index, "dimension sizes are : ", (dim_sizes(i), i=1, rank), & 
        &   "data type is : ", data_type, "number of attributes is : ", n_attrs

do i = 1 , rank
start(i) = 0
edges(i) = dim_sizes(i)
stride(i) = 1
end do

allocate(cloud_top_temperature_1km(dim_sizes(1),dim_sizes(2)),STAT=AllocateStatus)
IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
status = sfrdata( sds_id, start, stride, edges, cloud_top_temperature_1km )

do i = 1, 100
    write(6,*) cloud_top_temperature_1km(i,100)
end do

!----------------------------------------------------------------------------------------!
! Example of how to get SDS attributes

do attr_index = 0, n_attrs - 1
    status = sfgainfo(sds_id, attr_index, attr_name, data_type, n_values)
    write(6,*) attr_name, data_type, n_values
end do

attr_index = sffattr(sds_id, 'scale_factor')
write(6,*) attr_index
status = sfrattr(sds_id, attr_index, scale_factor)
write(6,*) scale_factor

attr_index = sffattr(sds_id, 'add_offset')
write(6,*) attr_index
status = sfrattr(sds_id, attr_index, add_offset)
write(6,*) add_offset

do i = 1, 10
    write(6,*) ( cloud_top_temperature_1km(i,100) - add_offset) * scale_factor
end do


!----------------------------------------------------------------------------------------!
! Example of how to extract information from a Byte

index = sfn2index(sd_id, 'Cloud_Mask_1km')

write(6,*) 'Cloud_Mask_1km index', index

sds_id = sfselect(sd_id, index)
status = sfginfo( sds_id , name , rank , dim_sizes , data_type , n_attrs )

sds_name = name(1:len(name))

do i = 1 , rank
start(i) = 0
edges(i) = dim_sizes(i)
stride(i) = 1
end do

allocate(Cloud_Mask_1km(dim_sizes(1),dim_sizes(2),dim_sizes(3)),STAT=AllocateStatus)
IF (AllocateStatus /= 0) STOP "*** Not enough memory Cloud_Mask_1km***"
status = sfrdata( sds_id, start, stride, edges, Cloud_Mask_1km )

write(6,*) 'READ MODIS Cloud Mask Example : '

do i = 1, 100

    cloud_mask_status_flag = ibits(Cloud_Mask_1km(1,i,1),0,1)
    cloud_mask = ibits(Cloud_Mask_1km(1,i,1),1,2)

    write(6,*) cloud_mask_status_flag, cloud_mask

end do

!----------------------------------------------------------------------------------------!
! deallocate

deallocate(cloud_top_temperature_1km, STAT = DeAllocateStatus)
if(DeAllocateStatus/=0) STOP

deallocate(Cloud_Mask_1km, STAT = DeAllocateStatus)
if(DeAllocateStatus/=0) STOP

!----------------------------------------------------------------------------------------!
! Close hdf file

status = sfendacc(sds_id)
status = sfend(sd_id)

!----------------------------------------------------------------------------------------!

end program how_to_read_modis_hdf4
