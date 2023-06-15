import constants

proc main
begin
	if constants::Pi[] + constants::Tau[] + constants::Phi[] != 11
	begin
		exit 1ss
	end if
end proc
