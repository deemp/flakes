// SPDX-License-Identifier: MIT LICENSE

pragma solidity ^0.8.13;

import "./Rewards.sol";
import "./Collection.sol";
import "hardhat/console.sol";

contract NFTStaking is Ownable, IERC721Receiver {
    struct Vault {
        string name;
        NFTCollection collection;
        Rewards rewardsToken;
        FullCollection fullCollection;
        mapping(address => OwnerVault) ownedBy;
    }

    struct OwnerVault {
        uint16 totalStaked;
        // earned before last collection staking
        uint totalReward;
    }

    struct FullCollection {
        // last collection staking timestamp
        uint stakedAt;
        address owner;
    }

    mapping(address => OwnerVault) initOwnerVault;

    Vault[] public multiVault;

    constructor() {
        initOwnerVault[address(0)] = OwnerVault({
            totalStaked: 0,
            totalReward: 0
        });
    }

    // struct to store a stake's token, owner, and earning values
    struct Stake {
        uint tokenID;
        uint timestamp;
        address owner;
    }

    uint public totalStaked;
    mapping(uint => mapping(uint => Stake)) public stakeMultiVault;
    event NFTStaked(address owner, uint tokenID, uint value);
    event NFTUnstaked(address owner, uint tokenID, uint value);
    event Claimed(address owner, uint amount);

    function addVault(
        NFTCollection nftCollection,
        Rewards rewardsToken,
        string calldata name
    ) public {
        Vault storage newVault = multiVault.push();
        newVault.collection = nftCollection;
        newVault.rewardsToken = rewardsToken;
        newVault.name = name;
        newVault.fullCollection = initFullCollection;
    }

    function stake(uint vaultID, uint[] calldata tokenIDs)
        external
        nonZeroAddress(msg.sender, "Cannot stake")
    {
        address account = msg.sender;
        for (uint i = 0; i < tokenIDs.length; i++) {
            uint tokenID = tokenIDs[i];
            require(
                multiVault[vaultID].collection.ownerOf(tokenID) == account,
                notOwner(account, tokenID)
            );
            require(
                stakeMultiVault[vaultID][tokenID].tokenID == 0,
                "already staked"
            );

            multiVault[vaultID].ownedBy[account].totalStaked += 1;
            totalStaked += 1;

            console.log("total staked %o", totalStaked);

            multiVault[vaultID].collection.safeTransferFrom(
                account,
                address(this),
                tokenID
            );
            emit NFTStaked(account, tokenID, block.timestamp);

            stakeMultiVault[vaultID][tokenID] = Stake({
                owner: account,
                tokenID: uint24(tokenID),
                timestamp: uint48(block.timestamp)
            });
        }
        if (
            multiVault[vaultID].ownedBy[account].totalStaked ==
            multiVault[vaultID].collection.maxSupply()
        ) {
            multiVault[vaultID].fullCollection = FullCollection({
                owner: account,
                stakedAt: block.timestamp
            });
        }
    }

    string currentContract = "NFTStaking:";

    modifier nonZeroAddress(address account, string memory message) {
        require(
            account != zeroAddress,
            fmt3(currentContract, "zero address.", message)
        );
        _;
    }
    address zeroAddress = address(0);

    modifier isValidVaultID(uint vaultID, string memory message) {
        require(
            vaultID > 0 && vaultID < multiVault.length,
            fmt3(currentContract, "invalid vault ID.", message)
        );
        _;
    }

    FullCollection initFullCollection =
        FullCollection({owner: zeroAddress, stakedAt: 0});

    function _unstakeMany(
        address account,
        uint vaultID,
        uint[] calldata tokenIDs
    )
        internal
        nonZeroAddress(account, "Cannot unstake tokens")
        isValidVaultID(vaultID, "Cannot unstake tokens")
    {
        for (uint i = 0; i < tokenIDs.length; i++) {
            uint tokenID = tokenIDs[i];
            require(
                stakeMultiVault[vaultID][tokenID].tokenID != 0,
                fmt3("token", Strings.toString(tokenID), "is not staked")
            );
            Stake memory stake_ = stakeMultiVault[vaultID][tokenID];
            require(stake_.owner == msg.sender, notOwner(msg.sender, tokenID));

            multiVault[vaultID].ownedBy[account].totalStaked -= 1;
            totalStaked -= 1;

            // if an item was unstaked
            // this is not a collection that is staked
            multiVault[vaultID].fullCollection = FullCollection({
                owner: zeroAddress,
                stakedAt: 0
            });

            delete stakeMultiVault[vaultID][tokenID];
            emit NFTUnstaked(account, tokenID, block.timestamp);

            multiVault[vaultID].collection.transferFrom(
                address(this),
                account,
                tokenID
            );
        }
    }

    function claim(uint[] calldata tokenIDs, uint vaultID) external {
        _claim(msg.sender, vaultID, tokenIDs, false);
    }

    function claimAddressVaultTokens(
        address account,
        uint vaultID,
        uint[] calldata tokenIDs
    ) external {
        _claim(account, vaultID, tokenIDs, false);
    }

    function unstake(uint[] calldata tokenIDs, uint vaultID) external {
        _claim(msg.sender, vaultID, tokenIDs, true);
    }

    function notOwner(address addr, uint tokenID)
        internal
        pure
        returns (string memory)
    {
        return
            fmt3(
                Strings.toHexString(addr),
                "is not an owner of",
                Strings.toString(tokenID)
            );
    }

    function fmt3(
        string memory a,
        string memory b,
        string memory c
    ) internal pure returns (string memory) {
        return string(abi.encodePacked(a, " ", b, " ", c));
    }

    function fmt2(string memory a, string memory b)
        internal
        pure
        returns (string memory)
    {
        return string(abi.encodePacked(a, " ", b));
    }

    struct RewardRate {
        uint itemMinute;
        uint collectionMinute;
    }

    RewardRate rewardRate =
        RewardRate({itemMinute: 100, collectionMinute: 10000});

    function _rewardItem(uint time, uint timeStaked)
        internal
        view
        returns (uint)
    {
        return
            (rewardRate.itemMinute * 1 ether * (time - timeStaked)) / 1 minutes;
    }

    function rewardItem(uint timeStaked) internal view returns (uint) {
        return _rewardItem(block.timestamp, timeStaked);
    }

    function _rewardCollection(uint time, uint timeStaked)
        internal
        view
        returns (uint)
    {
        return
            (_rewardItem(time, timeStaked) / rewardRate.itemMinute) *
            rewardRate.collectionMinute;
    }

    function rewardCollection(uint timeStaked) internal view returns (uint) {
        return _rewardCollection(block.timestamp, timeStaked);
    }

    // function rewardCollection()

    function _claim(
        address account,
        uint vaultID,
        uint[] calldata tokenIDs,
        bool _unstake
    )
        internal
        nonZeroAddress(account, "Cannot claim")
        isValidVaultID(vaultID, "Cannot claim")
    {
        uint earned = 0;
        if (multiVault[vaultID].fullCollection.owner == account) {
            earned += multiVault[vaultID].ownedBy[account].totalReward;
            earned += rewardCollection(
                multiVault[vaultID].fullCollection.stakedAt
            );

            // restart reward timer
            multiVault[vaultID].ownedBy[account].totalReward = 0;
            multiVault[vaultID].fullCollection.stakedAt = block.timestamp;
        } else {
            for (uint i = 0; i < tokenIDs.length; i++) {
                uint tokenID = tokenIDs[i];
                Stake memory stake_ = stakeMultiVault[vaultID][tokenID];
                require(stake_.owner == account, notOwner(account, tokenID));
                uint stakedAt = stake_.timestamp;

                earned += rewardItem(stakedAt);

                // restart reward timer
                stake_.timestamp = block.timestamp;
                stakeMultiVault[vaultID][tokenID] = stake_;
            }
        }
        if (earned > 0) {
            multiVault[vaultID].rewardsToken.mint(account, earned);
        }
        if (_unstake) {
            _unstakeMany(account, vaultID, tokenIDs);
        }
        emit Claimed(account, earned);
    }

    function earningInfo(address account, uint vaultID)
        external
        view
        nonZeroAddress(account, "Cannot get earning info")
        isValidVaultID(vaultID, "Cannot get earning info")
        returns (uint[2] memory info)
    {
        uint tokenID;
        uint totalScore = 0;
        uint earned = 0;
        Stake memory staked = stakeMultiVault[vaultID][tokenID];
        uint stakedAt = staked.timestamp;
        earned += (100000 ether * (block.timestamp - stakedAt)) / 1 days;
        uint earnRatePerSecond = (totalScore * 1 ether) / 1 days;
        earnRatePerSecond = earnRatePerSecond / 100000;
        // earned, earnRatePerSecond
        return [earned, earnRatePerSecond];
    }

    // should never be used inside of transaction because of gas fee
    function balanceOf(address account, uint vaultID)
        public
        view
        returns (uint)
    {
        uint balance = 0;
        Vault storage vault = multiVault[vaultID];
        uint supply = vault.collection.totalSupply();
        for (uint i = 1; i <= supply; i++) {
            if (stakeMultiVault[vaultID][i].owner == account) {
                balance += 1;
            }
        }
        return balance;
    }

    // should never be used inside of transaction because of gas fee
    function tokensOfOwnerVault(address account, uint vaultID)
        public
        view
        returns (uint[] memory ownerTokens)
    {
        Vault storage vault = multiVault[vaultID];
        uint supply = vault.collection.totalSupply();
        uint[] memory tmp = new uint[](supply);

        uint index = 0;
        for (uint tokenID = 1; tokenID <= supply; tokenID++) {
            if (stakeMultiVault[vaultID][tokenID].owner == account) {
                tmp[index] = stakeMultiVault[vaultID][tokenID].tokenID;
                index += 1;
            }
        }

        uint[] memory tokens = new uint[](index);
        for (uint i = 0; i < index; i++) {
            tokens[i] = tmp[i];
        }

        return tokens;
    }

    function onERC721Received(
        address,
        address from,
        uint,
        bytes calldata
    )
        external
        view
        override
        nonZeroAddress(from, "Cannot send nfts to Vault directly")
        returns (bytes4)
    {
        return IERC721Receiver.onERC721Received.selector;
    }
}
