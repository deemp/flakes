pragma solidity ^0.4.24;

contract TimeLockFixed {
    mapping(address => uint) public balances;
    mapping(address => uint) public lockTime;

    function deposit() public payable {
        balances[msg.sender] += msg.value;
        lockTime[msg.sender] = now + 1 weeks;
    }

    function increaseLockTime(uint _secondsToIncrease) public {
        uint c = lockTime[msg.sender] + _secondsToIncrease;
        require(c >= lockTime[msg.sender], "Overflow encountered");
        lockTime[msg.sender] = c;
    }

    function withdraw() public {
        require(balances[msg.sender] > 0, "Error: empty deposit balance");
        require(now > lockTime[msg.sender], "Error: lock time hasn't yet expired");
        msg.sender.transfer(balances[msg.sender]);
        balances[msg.sender] = 0;
    }
}
