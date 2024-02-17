const Cinco = 5
const Zero = 0ss

const begin
	Seis = 6
	Sete = 7
end const

proc main
var a
begin
	set a = Cinco
	if a != 5 begin
		exit 1ss
	end if
	set a = Seis
	if a != 6 begin
		exit 1ss
	end if
	set a = Sete
	if a != 7 begin
		exit 1ss
	end if

	exit Zero
end proc
