// SPDX-License-Identifier: MIT LICENSE

// SPDX-License-Identifier: MIT
pragma solidity 0.8.4;

import "https://github.com/net2devcrypto/n2dstaking/N2DRewards.sol";
import "https://github.com/net2devcrypto/n2dstaking/Collection.sol";

abstract contract NFTStaking is Ownable, IERC721Receiver {
    uint256 public totalStaked;

    struct Stake {
        // 24 - allowed number of digits in a token id
        // this is bound by token supply
        // tokenIds start from 1
        uint24 tokenId;
        // capture the staking moment to start generating the rewards
        uint48 timestamp;
        address owner;
    }

    // when nft is staked
    // there can be a particular value on the nft
    event NFTStaked(address owner, uint256 tokenId, uint256 value);
    event NFTUnstaked(address owner, uint256 tokenId, uint256 value);

    // amount of tokens issued as a reward
    event Claimed(address owner, uint256 amount);

    // output of the Collection SC
    Collection nft;
    // Rewards Token SCk
    N2DRewards token;

    // get stake from token id
    mapping(uint256 => Stake) public vault;

    function stake(uint256[] calldata tokenIds) external {
        for (uint256 i = 0; i < tokenIds.length; i++) {
            uint256 tokenId = tokenIds[i];
            // check that owner wants to stake
            require(nft.ownerOf(tokenId) == msg.sender, "not your token");
            // check that we don't have this token in our vault
            require(vault[tokenId].tokenId == 0, "already staked");

            // should only increment if stake
            totalStaked += 1;
            // transfer token into our collection
            // via Collection SC
            nft.transferFrom(msg.sender, address(this), tokenId);

            // block.timestamp is in seconds
            emit NFTStaked(msg.sender, tokenId, block.timestamp);

            // save token into our vault
            vault[tokenId] = Stake({
                owner: msg.sender,
                tokenId: uint24(tokenId),
                timestamp: uint48(block.timestamp)
            });
        }
    }

    function _unstakeMany(address account, uint256[] calldata tokenIds)
        internal
    {
        for (uint256 i = 0; i < tokenIds.length; i++) {
            uint256 tokenId = tokenIds[i];
            Stake memory staked = vault[tokenId];
            require(staked.owner == msg.sender, "not an owner");

            totalStaked -= 1;

            delete vault[tokenId];
            emit NFTUnstaked(account, tokenId, block.timestamp);
            nft.transferFrom(address(this), account, tokenId);
        }
    }

    function claim(uint256[] calldata tokenIds) external {
        _claim(msg.sender, tokenIds, false);
    }

    uint256 mul = 1000;

    function rewardFormula(uint48 time) internal view returns (uint256) {
        return (mul * 1 ether * (block.timestamp - time)) / 1 days;
    }

    function mintReward(uint256 val) internal view returns (uint256) {
        return val / mul;
    }

    function _claim(
        address account,
        uint256[] calldata tokenIds,
        bool _unstake
    ) internal {
        uint256 earned = 0;

        for (uint256 i = 0; i < tokenIds.length; i++) {
            uint256 tokenId = tokenIds[i];
            Stake memory staked = vault[tokenId];
            require(staked.owner == account, "not an owner");

            uint48 stakedAt = staked.timestamp;

            // reward formula
            earned += rewardFormula(stakedAt);

            vault[tokenId] = Stake({
                owner: account,
                tokenId: uint24(tokenId),
                timestamp: uint48(block.timestamp)
            });
        }

        if (earned > 0) {
            earned /= mul;
            token.mint(account, earned);
        }

        if (_unstake) {
            _unstakeMany(account, tokenIds);
        }

        emit Claimed(account, earned);
    }

    // get staking information
    // TODO fix
    function earningInfo(uint256[] calldata tokenIds)
        external
        view
        returns (uint256[] memory info)
    {
        uint256[] memory earned = new uint256[](tokenIds.length);
        for (uint256 i = 0; i < tokenIds.length; i++) {
            uint256 tokenId = tokenIds[i];
            Stake memory staked = vault[tokenId];
            earned[i] = rewardFormula(staked.timestamp);
        }
        return earned;
    }

    function balanceOf(address account) public view returns (uint256) {
        uint256 balance = 0;
        uint256 supply = nft.totalSupply();
        for (uint256 i = 1; i <= supply; i++) {
            if (vault[i].owner == account) {
                balance += 1;
            }
        }
        return balance;
    }

    function tokensOfOwner(address account)
        public
        view
        returns (uint256[] memory ownerTokens)
    {
        uint256 supply = nft.totalSupply();
        uint256[] memory tmp = new uint256[](supply + 1);

        // write tokens of an account into tmp
        uint256 index = 0;
        for (uint tokenId = 1; tokenId <= supply; tokenId++) {
            if (vault[tokenId].owner == account) {
                tmp[index] = vault[tokenId].tokenId;
                index += 1;
            }
        }

        // copy tokens from tmp
        uint256[] memory tokens = new uint256[](index);
        for (uint i = 0; i < index; i++) {
            tokens[i] = tmp[i];
        }

        return tokens;
    }

    function onERC721Received(
        address,
        address from,
        uint256,
        bytes calldata
    ) external pure override returns (bytes4) {
        require(from == address(0x0), "Cannot send nfts to Vault directly");
        return IERC721Receiver.onERC721Received.selector;
    }
}
