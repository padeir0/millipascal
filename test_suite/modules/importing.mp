import constants

proc main
begin
	if constants::Pi[] + constants::Tau[] + constants::Phi[] != 11
	begin
		exit 1r
	end if
end proc
